{-# language OverloadedStrings #-}
module Main where

import Data.Aeson (ToJSON(..), encode)
import Data.ByteString (ByteString)
import Data.String.Conversions
import Data.Text
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import System.IO

import Redsift.DB

main :: IO ()
main = do
    let port = 9000
    hPutStrLn stderr ("attempting to listen on port " ++ show port)
    run port redsiftApp

redsiftApp :: Application
redsiftApp request =
    dispatch (requestMethod request) (pathInfo request) (queryString request)

dispatch :: Method -> [Text] -> Query -> IO Response
dispatch "GET" ["table", "list"] _ = do
    tables <- allTables
    return $ responseLBS ok200 [] (encode (toJSON tables))
dispatch "GET" ["query"] queryString =
    queryVarRequired queryString "q" $ \ q -> do
        result <- query (cs q) 100
        return $ responseLBS ok200 [] (encode (toJSON result))
dispatch _ _ _ = return $ responseLBS notFound404 [] "404 not found"

queryVarRequired :: Query -> ByteString -> (ByteString -> IO Response) -> IO Response
queryVarRequired query key cont = case lookup key query of
    Just (Just value) -> cont value
    _ -> return $ responseLBS badRequest400 [] ("missing query var: " <> cs key)
