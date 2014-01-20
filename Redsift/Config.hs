{-# language ScopedTypeVariables, OverloadedStrings #-}

module Redsift.Config where

import Control.Applicative
import Data.Yaml

data RedsiftConfig = RedsiftConfig {
    appPort :: Int,
    rowLimit :: Int,
    dbHost :: String,
    dbPort :: Int,
    dbUser :: String,
    dbName :: String,
    s3Access :: String,
    s3Secret :: String,
    s3Bucket :: String,
    exportExpiry :: Int, -- in seconds
    emailAccount :: String,
    emailPassword :: String
  }
    deriving Show

instance FromJSON RedsiftConfig where
    parseJSON (Object m) = RedsiftConfig <$>
        m .: "appPort" <*>
        m .: "rowLimit" <*>
        m .: "dbHost" <*>
        m .: "dbPort" <*>
        m .: "dbUser" <*>
        m .: "dbName" <*>
        m .: "s3Access" <*>
        m .: "s3Secret" <*>
        m .: "s3Bucket" <*>
        m .: "exportExpiry" <*>
        m .: "emailAccount" <*>
        m .: "emailPassword"
    parseJSON x = fail ("not an object: " ++ show x)

-- * config files
readRedsiftConfig :: IO RedsiftConfig
readRedsiftConfig =
    either (error . show) id <$>
    decodeFileEither "./redsift.config"
