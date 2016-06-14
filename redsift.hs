{-# language OverloadedStrings #-}
module Main where

import Control.Exception
import Data.ByteString (ByteString)
import Data.Aeson (ToJSON(..), encode)
import Data.Maybe
import Data.String.Conversions
import Options.Applicative
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Application.Static
import Network.Wai.UrlMap
import Safe
import System.Directory
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
    info (helper <*> (Options <$> cfg)) fullDesc
  where
    cfg :: Parser (Maybe FilePath)
    cfg = mkOptional $ strOption (
        long "config" <>
        short 'c' <>
        metavar "CONFIGFILE" <>
        help ("config file path (default: " ++ defaultConfigFile ++ ")"))

    mkOptional :: Parser a -> Parser (Maybe a)
    mkOptional p = (Just <$> p) <|> pure Nothing

defaultConfigFile :: FilePath
defaultConfigFile = "./Config/redsift.config"

-- | Chooses './www' if that exists, uses cabal's data-files mechanism
-- otherwise.
getDocumentRoot :: IO FilePath
getDocumentRoot = do
    wwwExists <- doesDirectoryExist "www"
    if wwwExists then return "www"
    else do
        cabalDataDir <- getDataDir
        cabalDataDirExists <- doesDirectoryExist cabalDataDir
        if cabalDataDirExists
            then return (cabalDataDir </> "www")
            else throwIO (ErrorCall "directory for static files not found.")


-- * main entry function

main :: IO ()
main = do
    options <- execParser optionsParser
    redsiftConfig <- readRedsiftConfig (fromMaybe defaultConfigFile (config options))
    let port = appPort $ app redsiftConfig
    documentRoot <- getDocumentRoot
    hPutStrLn stderr ("serving static files from " ++ documentRoot)
    let settings =
            setPort port $
            setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
            setFdCacheDuration 0 $
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
    staticApp (defaultFileServerSettings (documentRoot ++ "/"))

-- * api
apiApp :: RedsiftConfig -> Application
apiApp redsiftConfig request respond = do
    let dbConfig = db redsiftConfig
    case requestMethod request of
        "GET" -> case pathInfo request of
            ["table", "list"] -> do
                tables <- allTables dbConfig
                respond $ responseLBS ok200 [] (encode (toJSON tables))
            ["query"] -> queryVarRequired (queryString request) "q" $ \ q -> do
                result <- query dbConfig (getEmail request) (toRsQuery $ cs q) (app redsiftConfig)
                respond $ responseLBS ok200 [] (encode (toJSON result))
            ["export"] -> queryVarRequired (queryString request) "e" $ \ e ->
                queryVarRequired (queryString request) "n" $ \ n -> do
                    result <- export dbConfig (getEmail request) (cs n) (toRsQuery $ cs e) (s3 redsiftConfig) (email redsiftConfig)
                    respond $ responseLBS ok200 [] (encode (toJSON result))
            _ -> respond notFoundError
        _ -> respond notFoundError
    where
        getEmail :: Request -> Address
        getEmail r = Address Nothing $
            cs $ snd $ headNote "'From' header not set" $ filter (\h -> fst h == "From") (requestHeaders r)
        notFoundError = responseLBS notFound404 [] "404 not found"

        queryVarRequired :: Query -> ByteString -> (ByteString -> IO ResponseReceived) -> IO ResponseReceived
        queryVarRequired q key cont = case lookup key q of
            Just (Just v) -> cont v
            _ -> respond $ responseLBS badRequest400 [] ("missing query var: " <> cs key)
