{-# language OverloadedStrings #-}
module Main where

import Data.Aeson (ToJSON(..), encode)
import Data.ByteString (ByteString)
import Data.Maybe
import Data.String.Conversions
import Filesystem.Path.CurrentOS (decodeString)
import Options.Applicative
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp hiding (Connection)
import Network.Wai.Application.Static
import Network.Wai.UrlMap
import Safe
import System.FilePath
import System.IO

import Paths_redsift

import Redsift.DB
import Redsift.Exception
import Redsift.Config


-- * command line options

data Options = Options {
    config :: Maybe FilePath
  }

optionsParser :: ParserInfo Options
optionsParser =
    info (helper <*> (Options <$> config)) fullDesc
  where
    config :: Parser (Maybe FilePath)
    config = mkOptional $ strOption (
        long "config" <>
        short 'c' <>
        metavar "CONFIGFILE" <>
        help ("config file path (default: " ++ defaultConfigFile ++ ")"))

    mkOptional :: Parser a -> Parser (Maybe a)
    mkOptional p = (Just <$> p) <|> pure Nothing

defaultConfigFile :: FilePath
defaultConfigFile = "./Config/redsift.config"


-- * main entry function

main :: IO ()
main = do
    options <- execParser optionsParser
    redsiftConfig <- readRedsiftConfig (fromMaybe defaultConfigFile (config options))
    let port = appPort $ app redsiftConfig
    documentRoot <- (</> "www") <$> getDataDir
    let settings =
            setPort port $
            setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
            defaultSettings
    runSettings settings $ handleApp errorHandler $
        mapUrls (redsiftApp redsiftConfig documentRoot)

-- | Routing between static files and the API
redsiftApp :: RedsiftConfig -> FilePath -> UrlMap
redsiftApp redsiftConfig documentRoot =
    mount "api" (apiApp redsiftConfig) <|>
    mountRoot (fileServerApp documentRoot)

-- * file serving
fileServerApp :: FilePath -> Application
fileServerApp documentRoot =
    staticApp (defaultFileServerSettings (decodeString (documentRoot ++ "/")))

-- * api
apiApp :: RedsiftConfig -> Application
apiApp redsiftConfig request =
    let dbConfig = db redsiftConfig
    in case requestMethod request of
        "GET" -> case pathInfo request of
            ["table", "list"] -> do
                tables <- allTables dbConfig
                return $ responseLBS ok200 [] (encode (toJSON tables))
            ["query"] -> queryVarRequired (queryString request) "q" $ \ q -> do
                result <- query dbConfig (getEmail request) (cs q) (app redsiftConfig)
                return $ responseLBS ok200 [] (encode (toJSON result))
            ["export"] -> queryVarRequired (queryString request) "e" $ \ e ->
                queryVarRequired (queryString request) "n" $ \ n -> do
                    result <- export dbConfig (getEmail request) (cs n) (cs e) (s3 redsiftConfig) (gmail redsiftConfig)
                    return $ responseLBS ok200 [] (encode (toJSON result))
            _ -> return notFoundError
        _ -> return notFoundError
    where getEmail request = cs $ snd $ headNote "'From' header not set" $ filter (\header -> fst header == "From") (requestHeaders request)
          notFoundError = responseLBS notFound404 [] "404 not found"

queryVarRequired :: Query -> ByteString -> (ByteString -> IO Response) -> IO Response
queryVarRequired query key cont = case lookup key query of
    Just (Just value) -> cont value
    _ -> return $ responseLBS badRequest400 [] ("missing query var: " <> cs key)