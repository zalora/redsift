{-# LANGUAGE DeriveDataTypeable #-}
module Redsift.Exception where

import Control.Exception
import Data.Typeable
import Data.Monoid
import Network.Wai
import System.IO (hPutStrLn, stderr)
import Data.String.Conversions (cs)
import Network.HTTP.Types (internalServerError500)
import Database.PostgreSQL.Simple (SqlError(..))

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
mailUserExceptions :: EmailConfig -> Address -> IO a -> IO a
mailUserExceptions emailConfig recipient =
    handle $ \ e@(UserException m) -> do
        sendMail emailConfig recipient "Your Redsift Export Failed" $
            unlines $
                "We are very sorry, but your redsift export process did not go through successfully. Here's the error message:" :
                m :
                "" :
                "Feel free to get in touch with the data science team." :
                []
        throwIO e

sqlToUser :: SqlError -> UserException
sqlToUser (SqlError _ _ message detail hint) =
    UserException $ cs (message <<>> detail <<>> hint)
  where
    a <<>> b = a <> " " <> b

mapExceptionIO :: (Exception e1, Exception e2) => (e1 -> e2) -> IO a -> IO a
mapExceptionIO f =
    handle $ \ x -> throwIO $ f x
