{-# LANGUAGE OverloadedStrings #-}
module Redsift.DB (allTables, query) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.UTF8 as BU
import Data.List (foldl')
import Data.Maybe (fromJust)
import qualified Database.PostgreSQL.LibPQ as PQ
import Database.PostgreSQL.Simple hiding (query)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.String.Utils (strip, join)
import Data.Char (toLower)
import Data.List.Split (splitOn)

host = "redcat.zalora.com"
port = 5439
user = "dsteam"
db = "redcat"

connect' = connect defaultConnectInfo {connectHost = host, connectPort = port, connectUser = user, connectDatabase = db}

allTables :: IO (Map.Map String [(String, Bool)])
allTables = (foldl' f Map.empty) `fmap` (flip query_ "SELECT table_schema, table_name, table_type = 'VIEW' FROM information_schema.tables" =<< connect')
 where f tuples (schema, name, type') = Map.insertWith (++) schema [(name, type')] tuples

-- As far as I can tell, postgresql-simple doesn't give us access to
-- the underlying LibPQ result or allow us to access the column names,
-- which is what I need here. I also want to get the results as
-- strings rather than as automatically-type-converted values, so I'm
-- using LibPQ directly here.
--
-- We have to return Aeson values here since there's no way of
-- representing Null (especially in homogenous lists) without them.
query :: String -> Integer -> IO Aeson.Value
query q limit = do
  c <- PQ.connectdb $ BU.fromString $ "host=" ++ host ++ " port=" ++ show port ++ " dbname=" ++ db ++ " user=" ++ user
  r' <- PQ.exec c $ BU.fromString $ limitQuery limit q
  case r' of
    Nothing -> (error . BU.toString . fromJust) `fmap` PQ.errorMessage c
    Just r -> do
      status <- PQ.resultStatus r
      case status of
        PQ.CommandOk -> return Aeson.Null -- shouldn't happen because web users shouldn't have write access
        PQ.TuplesOk -> tuples r
        _ -> (error . BU.toString . fromJust) `fmap` PQ.resultErrorMessage r
 where tuples r = do
         nTuples <- PQ.ntuples r
         nFields <- PQ.nfields r
         names <- mapM (\x -> fromJust `fmap` PQ.fname r x) [0 .. nFields - 1]
         rows <- mapM (getRow r nFields) [0 .. nTuples - 1]
         return $ Aeson.toJSON $ [map (Aeson.String . Text.pack . BU.toString) names] ++ rows
       getRow r nFields rowNum = mapM (\x -> toValue `fmap` PQ.getvalue r rowNum x) [0 .. nFields - 1]
       toValue v = case v of
                     Nothing -> Aeson.Null
                     Just s -> Aeson.String $ Text.pack $ BU.toString s

-- in case User's Defined Query doesn't limit number of rows, or return too many rows than allowed
limitQuery :: Integer -> String -> String
limitQuery limit q =  join ";" $ map (applyLimit limit) $ splitQuery q

applyLimit :: Integer -> String -> String
applyLimit limit q = let qAsList = words q in
                     case (map toLower (qAsList !! (length qAsList - 2))) of
                       "limit" -> case (read (qAsList !! (length qAsList - 1))) > limit of
                                    True -> unwords $ (init qAsList) ++ [show limit]
                                    False -> unwords $ qAsList
                       _ -> unwords $ qAsList ++ ["limit", show limit]

splitQuery :: String -> [String]
splitQuery q = filter (not . null) $ map strip $ splitOn ";" q
