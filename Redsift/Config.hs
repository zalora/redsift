{-# language ScopedTypeVariables, OverloadedStrings, DataKinds,
             FlexibleInstances #-}

module Redsift.Config (
    module Redsift.Config,
    Address(..),
  ) where

import Control.Applicative
import Data.Configurator as C
import Data.Configurator.Types as C
import Data.Text
import Network.Mail.Mime (Address(..))


-- * Config Data Type
data RedsiftConfig = RedsiftConfig {
    app :: AppConfig,
    db :: DbConfig,
    s3 :: S3Config,
    email :: EmailConfig
  } deriving Show

data AppConfig = AppConfig {
    appPort :: Int,
    rowLimit :: Int
  } deriving Show

data DbConfig = DbConfig {
    dbConnectionString :: String
  } deriving Show

data S3Config = S3Config {
    bucket :: String,
    access :: String,
    secret :: String,
    expiry :: Int -- in seconds
  } deriving Show

data EmailConfig = EmailConfig {
    account :: Address
  }

instance Show EmailConfig where
    show (EmailConfig a) = show (addressEmail a)


class FromGroup a where
    getFromGroup :: Config -> IO a

instance FromGroup AppConfig where
    getFromGroup c = AppConfig <$>
        require c "port" <*>
        require c "rowLimit"

instance FromGroup DbConfig where
    getFromGroup c = DbConfig <$>
        require c "redcatConnectionString"

instance FromGroup S3Config where
    getFromGroup c = S3Config <$>
        require c "bucket" <*>
        require c "access" <*>
        require c "secret" <*>
        require c "expiry"

instance FromGroup EmailConfig where
    getFromGroup c = EmailConfig . Address Nothing <$>
        require c "sender"


-- * read config file
readRedsiftConfig :: FilePath -> IO RedsiftConfig
readRedsiftConfig configFile = do
    c <- load [Required configFile]
    let l :: FromGroup a => Text -> IO a
        l n = getFromGroup (subconfig n c)
    RedsiftConfig <$>
        l "app" <*>
        l "db" <*>
        l "s3" <*>
        l "email"
