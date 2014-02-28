{-# LANGUAGE DeriveDataTypeable #-}
module Redsift.Exception where

import Control.Exception
import Data.Typeable
import Network.Wai
import System.IO (hPutStrLn, stderr)
import Data.String.Conversions (cs)
import Network.HTTP.Types (internalServerError500)

import Redsift.Config
import Redsift.Mail


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

-- Catches UserExceptions and mails them to the given receiver.
mailUserExceptions :: GmailConfig -> String -> IO a -> IO a
mailUserExceptions gmailConfig recipient =
    handle $ \ e@(UserException m) -> do
        sendMail gmailConfig recipient "Your Redsift Export Failed" $
            unlines $
                "We are very sorry, but your redsift export process did not go through successfully. Here's the error message:" :
                m :
                "" :
                "Feel free to get in touch with the data science team." :
                []
        throwIO e
