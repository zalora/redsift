{-# language OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>), (<|>))
import Data.Aeson (ToJSON(..), encode)
import Data.ByteString (ByteString)
import Data.String.Conversions
import Database.PostgreSQL.Simple hiding (query, Query)
import Filesystem.Path.CurrentOS (decodeString)
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

main :: IO ()
main = do
    redsiftConfig <- readRedsiftConfig
    let port = appPort $ app redsiftConfig
    documentRoot <- (</> "www") <$> getDataDir
    let settings = defaultSettings{
            settingsPort = port,
            settingsBeforeMainLoop =
                hPutStrLn stderr ("listening on port " ++ show port)
          }
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
    let (DbConfig host port database) = db redsiftConfig
        accountConfigs = accounts redsiftConfig
        groups = getGroups request
        user = getRedshiftUser groups accountConfigs
        connectInfo = defaultConnectInfo {
            connectHost = host,
            connectPort = fromIntegral port,
            connectUser = user,
            connectDatabase = database}
    in case requestMethod request of
        "GET" -> case pathInfo request of
            ["table", "list"] -> do
                tables <- allTables connectInfo
                return $ responseLBS ok200 [] (encode (toJSON tables))
            ["query"] -> queryVarRequired (queryString request) "q" $ \ q -> do
                result <- query connectInfo (cs q) (app redsiftConfig)
                return $ responseLBS ok200 [] (encode (toJSON result))
            ["export"] -> queryVarRequired (queryString request) "e" $ \ e ->
                queryVarRequired (queryString request) "n" $ \ n -> do
                    result <- export connectInfo (getEmail request) (cs n) (cs e) (s3 redsiftConfig) (gmail redsiftConfig)
                    return $ responseLBS ok200 [] (encode (toJSON result))
            _ -> return notFoundError
        _ -> return notFoundError
    where getEmail request = cs $ snd $ headNote "'From' header not set" $ filter (\header -> fst header == "From") (requestHeaders request)
          getGroups request = words $ cs $ snd $ headNote "'Groups' header not set" $ filter (\header -> fst header == "Groups") (requestHeaders request)
          getRedshiftUser groups accountConfigs
                | null accountConfigs = ""
                | (groupname (head accountConfigs)) `elem` groups = redcataccount (head accountConfigs)
                | otherwise = getRedshiftUser groups $ tail accountConfigs

          notFoundError = responseLBS notFound404 [] "404 not found"

queryVarRequired :: Query -> ByteString -> (ByteString -> IO Response) -> IO Response
queryVarRequired query key cont = case lookup key query of
    Just (Just value) -> cont value
    _ -> return $ responseLBS badRequest400 [] ("missing query var: " <> cs key)