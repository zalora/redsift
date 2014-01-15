{-# language OverloadedStrings, DeriveDataTypeable #-}
module Main where

import Control.Applicative ((<$>), (<|>))
import Control.Exception
import Data.Aeson (ToJSON(..), encode)
import Data.ByteString (ByteString)
import Data.String.Conversions
import Data.Text
import Data.Typeable
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
    run port $ handleApp errorHandler $
        mapUrls (redsiftApp documentRoot)

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


-- * exception handling (and throwing)

data UserException = UserException String
  deriving (Typeable, Show)

instance Exception UserException

throwUserException :: String -> IO a
throwUserException = throwIO . UserException

errorHandler :: UserException -> IO Response
errorHandler (UserException reason) = do
    let msg = "server error: " ++ reason
    hPutStrLn stderr msg
    return $ responseLBS internalServerError500 [] (cs msg)

-- | Wraps the request handling of the given application
-- to handle raised exceptions through the given error handler.
handleApp :: Exception e =>
    (e -> IO Response) -> Application -> Application
handleApp errorHandler app request =
    handle errorHandler (app request)
