{-# language OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>), (<|>))
import Data.Aeson (ToJSON(..), encode)
import Data.ByteString (ByteString)
import Data.String.Conversions
import Data.Text
import Filesystem.Path.CurrentOS (decodeString)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Application.Static
import Network.Wai.UrlMap
import System.FilePath
import System.IO

import Paths_redsift

import Redsift.DB

main :: IO ()
main = do
    let port = 9000
    hPutStrLn stderr ("attempting to listen on port " ++ show port)
    documentRoot <- (</> "www") <$> getDataDir
    run port $ mapUrls (redsiftApp documentRoot)

-- | Routing between static files and the API
redsiftApp :: FilePath -> UrlMap
redsiftApp documentRoot =
    mount "api" apiApp <|>
    mountRoot (fileServerApp documentRoot)


-- * file serving

fileServerApp :: FilePath -> Application
fileServerApp documentRoot =
    staticApp (defaultFileServerSettings (decodeString (documentRoot ++ "/")))


-- * api

apiApp :: Application
apiApp request = apiHandler (requestMethod request) (pathInfo request) (queryString request)

apiHandler :: Method -> [Text] -> Query -> IO Response
apiHandler "GET" ["table", "list"] _ = do
    tables <- allTables
    return $ responseLBS ok200 [] (encode (toJSON tables))
apiHandler "GET" ["query"] queryString =
    queryVarRequired queryString "q" $ \ q -> do
        result <- query (cs q) 100
        return $ responseLBS ok200 [] (encode (toJSON result))
apiHandler _ _ _ = return $ responseLBS notFound404 [] "404 not found"

queryVarRequired :: Query -> ByteString -> (ByteString -> IO Response) -> IO Response
queryVarRequired query key cont = case lookup key query of
    Just (Just value) -> cont value
    _ -> return $ responseLBS badRequest400 [] ("missing query var: " <> cs key)
