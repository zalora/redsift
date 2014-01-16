{-# LANGUAGE DeriveDataTypeable #-}
module Redsift.Exception where

import Control.Exception
import Data.Typeable
import Network.Wai
import System.IO (hPutStrLn, stderr)
import Data.String.Conversions (cs)
import Network.HTTP.Types (internalServerError500)

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