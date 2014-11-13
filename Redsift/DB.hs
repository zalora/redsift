{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Redsift.DB where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.UTF8 as BU
import Data.List (foldl', elemIndices, intercalate, delete)
import Data.Maybe (fromJust, fromMaybe)
import qualified Database.PostgreSQL.LibPQ as PQ
import Database.PostgreSQL.Simple as Simple
import Database.PostgreSQL.Simple.Internal (withConnection)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Char (isAlphaNum)
import Data.Time (getCurrentTime, utctDay, formatTime)
import Control.Concurrent (forkIO)
import Network.AWS.S3Bucket
import Network.AWS.AWSConnection
import System.Locale
import Control.Applicative
import Control.Exception (bracket)
import Safe
import Data.String
import Data.String.Conversions
import Text.Printf
import System.IO

import Redsift.Exception
import Redsift.SignUrl
import Redsift.Mail
import Redsift.Config

newtype RsQuery = RsQuery { queryStrings :: [String] }

toRsQuery :: String -> RsQuery
toRsQuery = RsQuery . lines

allTables :: DbConfig -> IO (Map.Map String [(String, Bool)])
allTables dbConfig = withDB dbConfig $ \db -> foldl' f Map.empty `fmap` query_ db "SELECT table_schema, table_name, table_type = 'VIEW' FROM information_schema.tables ORDER BY table_schema, table_name DESC"
 where f tuples (schema, name, type') = Map.insertWith (++) schema [(name, type')] tuples

-- As far as I can tell, postgresql-simple doesn't give us access to
-- the underlying LibPQ result or allow us to access the column names,
-- which is what I need here. I also want to get the results as
-- strings rather than as automatically-type-converted values, so I'm
-- using LibPQ directly here.
--
-- We have to return Aeson values here since there's no way of
-- representing Null (especially in homogenous lists) without them.

-- * issue general Query

query :: DbConfig -> Address -> RsQuery -> AppConfig -> IO Aeson.Value
query dbConfig user q (AppConfig _ rowLimit) = withDB dbConfig $ \db -> withConnection db $ \db' ->
  if isSingleQuery q then do
     wrappedQuery <- limitQuery rowLimit <$> prepareQuery user q
     r' <- PQ.exec db' . BU.fromString . unlines . queryStrings $ wrappedQuery
     case r' of
       Nothing -> do
         err <- (BU.toString . fromJust) `fmap` PQ.errorMessage db'
         throwUserException err
       Just r -> do
         status <- PQ.resultStatus r
         case status of
           PQ.CommandOk -> return Aeson.Null -- shouldn't happen because web users shouldn't have write access
           PQ.TuplesOk -> tuples r
           _ -> do
             err <- (BU.toString . fromJust) `fmap` PQ.resultErrorMessage r
             throwUserException err
  else throwUserException "Multiple queries is not allowed."
  where tuples r = do
          nTuples <- PQ.ntuples r
          nFields <- PQ.nfields r
          names <- mapM (\x -> fromJust `fmap` PQ.fname r x) [0 .. nFields - 1]
          rows <- mapM (getRow r nFields) [0 .. nTuples - 1]
          return $ Aeson.toJSON $ map (Aeson.String . Text.pack . BU.toString) names : rows
        getRow r nFields rowNum = mapM (\x -> toValue `fmap` PQ.getvalue r rowNum x) [0 .. nFields - 1]
        toValue v = case v of
                      Nothing -> Aeson.Null
                      Just s -> Aeson.String $ Text.pack $ BU.toString s

isSingleQuery :: RsQuery -> Bool
isSingleQuery RsQuery{ queryStrings } = noSemicolon || firstSemicolonIsLastChar
  where
    queryString              = intercalate "\n" queryStrings
    firstSemicolonIndex      = headMay . elemIndices ';' $ queryString
    lastIndex                = length queryString - 1
    noSemicolon              = ';' `notElem` queryString
    firstSemicolonIsLastChar = fromMaybe False $ (== lastIndex) <$> firstSemicolonIndex

-- * export CSV

export :: DbConfig -> Address -> String -> RsQuery -> S3Config -> EmailConfig -> IO Aeson.Value
export dbConfig recipient reportName q s3Config emailConfig = do
  s3Prefix <- createS3Prefix recipient $ filter isAlphaNum reportName -- prevent reportName to have non-alpha nor non-numeric character
  _ <- forkIO $ mailUserExceptions emailConfig recipient $ mapExceptionIO sqlToUser $
    withDB dbConfig $ \ db -> do
        escapedQuery <- withConnection db $ \ raw -> escapeRsQuery raw q
        case escapedQuery of
            Nothing -> throwUserException "query escaping failed"
            Just escapedQuery -> do
                unload <- unloadQuery s3Prefix s3Config <$> prepareQuery recipient escapedQuery
                _ <- Simple.execute_ db $ fromString unload
                processSuccessExport s3Prefix recipient s3Config emailConfig
  return "Your export request has been sent. The export URL will be sent to your email shortly."

escapeRsQuery :: PQ.Connection -> RsQuery -> IO (Maybe RsQuery)
escapeRsQuery conn RsQuery{ queryStrings } = fmap (toRsQuery . cs) <$> escapeQuery
  where
    escapeQuery = PQ.escapeStringConn conn (cs . unlines $ queryStrings)

-- Generate S3 Location, based on current Date, current Time, export Name, and recipient Email
createS3Prefix :: Address -> String -> IO String
createS3Prefix recipient reportName = do
  now <- getCurrentTime
  date <- fmap (show.utctDay) getCurrentTime
  return $ cs (addressEmail recipient) ++ "/" ++ date ++ "/" ++ reportName ++ "_" ++ formatTime defaultTimeLocale "%T" now ++ "_"

-- Returns an UNLOAD statement for a given query.
unloadQuery :: String -> S3Config -> RsQuery -> String
unloadQuery s3Prefix (S3Config bucket access secret _) RsQuery{ queryStrings } =
    "UNLOAD ('"
    ++ "\n"
    ++ unlines queryStrings
    ++ "\n"
    ++ "') to '"
    ++ "s3://" ++ bucket ++ "/" ++ s3Prefix
    ++ "' credentials 'aws_access_key_id="
    ++ access
    ++ ";aws_secret_access_key="
    ++ secret
    ++ "'ALLOWOVERWRITE GZIP"
    ++ " ADDQUOTES"
    ++ " PARALLEL OFF;"

-- Once Data is exported to S3, find the gz export, send email accordingly
processSuccessExport :: String -> Address -> S3Config -> EmailConfig -> IO ()
processSuccessExport s3Prefix recipient (S3Config bucket access secret expiry) mailConfig = do
    epoch <- read <$> formatTime defaultTimeLocale "%s" <$> getCurrentTime
    listResult <- listAllObjects (amazonS3Connection access secret) bucket (ListRequest s3Prefix ""  "" 0)
    case listResult of
      Left err -> throwUserException $ show err
      Right (result : _) ->
        let url = show $ signUrl (access, secret) bucket (key result) (epoch + fromIntegral expiry)
        in sendCSVExportMail mailConfig recipient url
      Right [] -> throwUserException ("redsift export: s3 file with the given prefix could not be found: " ++ s3Prefix)


-- * common

-- Adds a comment containing the sproxy user (email) and logs the query to stderr.
-- Every query sent to redcat has to pass this function.
prepareQuery :: Address -> RsQuery -> IO RsQuery
prepareQuery user q@RsQuery{ queryStrings } = do
    let withUserComment =
            printf "/* redsift query for user '%s' */ " (cs (addressEmail user) :: String) ++
            intercalate " " queryStrings
    hPutStrLn stderr ("redcat query: " ++ withUserComment)
    return q

-- Wrap user query with limit
limitQuery :: Int -> RsQuery -> RsQuery
limitQuery limit RsQuery{ queryStrings } = RsQuery $ ["SELECT * FROM ("] ++ map (delete ';') queryStrings ++ [") LIMIT " ++ show limit]

-- Make sure a connection is closed after we finished with the query.
withDB :: DbConfig -> (Connection -> IO a) -> IO a
withDB dbConfig =
    bracket
        (connectPostgreSQL (cs (dbConnectionString dbConfig)))
        close
