{-# LANGUAGE OverloadedStrings #-}
module Redsift.DB (allTables, query, export) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.UTF8 as BU
import Data.List (foldl', elemIndices)
import Data.Maybe (fromJust)
import qualified Database.PostgreSQL.LibPQ as PQ
import Database.PostgreSQL.Simple hiding (query)
import Database.PostgreSQL.Simple.Internal (withConnection)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Char (toLower)
import Data.Time (getCurrentTime)
import Control.Concurrent (forkIO)
import Redsift.Exception

allTables :: Connection -> IO (Map.Map String [(String, Bool)])
allTables db = (foldl' f Map.empty) `fmap` (query_ db "SELECT table_schema, table_name, table_type = 'VIEW' FROM information_schema.tables")
 where f tuples (schema, name, type') = Map.insertWith (++) schema [(name, type')] tuples

-- As far as I can tell, postgresql-simple doesn't give us access to
-- the underlying LibPQ result or allow us to access the column names,
-- which is what I need here. I also want to get the results as
-- strings rather than as automatically-type-converted values, so I'm
-- using LibPQ directly here.
--
-- We have to return Aeson values here since there's no way of
-- representing Null (especially in homogenous lists) without them.

query :: Connection -> String -> Int -> IO Aeson.Value
query db q limit = withConnection db $ \db' -> do
  if (isSingleQuery q) then do
      r' <- PQ.exec db' $ BU.fromString $ limitQuery limit q
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
  where isSingleQuery q
          | length (filter (== ';') q) == 0 = True           -- No semicolons
          | head (elemIndices ';' q) == length q - 1 = True -- Make sure nothing is behind the first semicolon
          | otherwise = False
        tuples r = do
          nTuples <- PQ.ntuples r
          nFields <- PQ.nfields r
          names <- mapM (\x -> fromJust `fmap` PQ.fname r x) [0 .. nFields - 1]
          rows <- mapM (getRow r nFields) [0 .. nTuples - 1]
          return $ Aeson.toJSON $ [map (Aeson.String . Text.pack . BU.toString) names] ++ rows
        getRow r nFields rowNum = mapM (\x -> toValue `fmap` PQ.getvalue r rowNum x) [0 .. nFields - 1]
        toValue v = case v of
                      Nothing -> Aeson.Null
                      Just s -> Aeson.String $ Text.pack $ BU.toString s

export :: Connection -> String -> String -> String -> String -> String -> String -> IO Aeson.Value
export db email name q bucket access secret = withConnection db $ \db' -> do
  s3 <- createS3Url bucket email name
  forkIO $ do
    r' <- PQ.exec db' $ BU.fromString $ unloadQuery q s3 access secret
    case r' of
      Nothing -> (error . BU.toString . fromJust) `fmap` PQ.errorMessage db'
      Just r -> putStrLn $ "IT WORKS!" ++ (show r) -- TODO: process the URL
  return "Your export request has been sent."
  where createS3Url bucket email name = do
          now <- fmap show getCurrentTime
          return $ bucket ++ "/" ++ email ++ "/" ++ name ++ now

-- Wrap normal query with UNLOAD statement and 2147483647 as MaxLimit so that the result will be just one file on S3
-- refer to: https://bitbucket.org/zalorasea/redsift/issue/3/export-to-csv
unloadQuery :: String -> String -> String -> String -> String
unloadQuery q s3 access secret = "UNLOAD ('SELECT * FROM ("
  ++ limitQuery 2147483647 q
  ++ " )') to '"
  ++ s3
  ++ "' credentials 'aws_access_key_id="
  ++ access
  ++ ";aws_secret_access_key="
  ++ secret
  ++ "' ALLOWOVERWRITE gzip;" -- TODO: read from config file

-- In case User's Defined Query doesn't limit number of rows, or return too many rows than allowed
-- For this method, query q is assumed to have AT MOST one semicolon,
-- otherwise it would result in error in the query function
limitQuery :: Int -> String -> String
limitQuery limit q = let qAsList = words $ filter (/=';') q in
                     case (map toLower (qAsList !! (length qAsList - 2))) of
                       "limit" -> case (read (qAsList !! (length qAsList - 1))) > limit of
                                    True -> unwords $ (init qAsList) ++ [show limit]
                                    False -> unwords $ qAsList
                       _ -> unwords $ qAsList ++ ["limit", show limit]