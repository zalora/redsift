{-# language ScopedTypeVariables, OverloadedStrings, DataKinds #-}

module Redsift.Config where

import Control.Applicative
import Data.Yaml

-- * Config Data Type
data RedsiftConfig = RedsiftConfig {
    app :: AppConfig,
    db :: DbConfig,
    s3 :: S3Config,
    gmail :: GmailConfig
}

data AppConfig = AppConfig {
    appPort :: Int,
    rowLimit :: Int
  } deriving Show

data DbConfig = DbConfig {
    host :: String,
    port :: Int,
    user :: String,
    database :: String
  } deriving Show

data S3Config = S3Config {
    bucket :: String,
    access :: String,
    secret :: String,
    expiry :: Int -- in seconds
  } deriving Show

data GmailConfig = GmailConfig {
    account :: String,
    password :: String
  } deriving Show

-- * FromJSON
instance FromJSON RedsiftConfig where
    parseJSON (Object m) = RedsiftConfig <$>
        m .: "app" <*>
        m .: "db" <*>
        m .: "s3" <*>
        m .: "gmail"
    parseJSON x = fail ("not an object: " ++ show x)       

instance FromJSON AppConfig where
    parseJSON (Object m) = AppConfig <$>
        m .: "appPort" <*>
        m .: "rowLimit"
    parseJSON x = fail ("not an object: " ++ show x)

instance FromJSON DbConfig where
    parseJSON (Object m) = DbConfig <$>
        m .: "host" <*>
        m .: "port" <*>
        m .: "user" <*>
        m .: "database"
    parseJSON x = fail ("not an object: " ++ show x)

instance FromJSON S3Config where
    parseJSON (Object m) = S3Config <$>
        m .: "bucket" <*>
        m .: "access" <*>
        m .: "secret" <*>
        m .: "expiry"
    parseJSON x = fail ("not an object: " ++ show x)

instance FromJSON GmailConfig where
    parseJSON (Object m) = GmailConfig <$>
        m .: "account" <*>
        m .: "password"
    parseJSON x = fail ("not an object: " ++ show x)

-- * read config file
readRedsiftConfig :: IO RedsiftConfig
readRedsiftConfig =
    either (error . show) id <$>
    decodeFileEither "./Config/redsift.config"
