{-# LANGUAGE DeriveDataTypeable #-}
module Redsift.Exception where

import Control.Exception
import Data.Typeable

data UserException = UserException String
  deriving (Typeable, Show)

instance Exception UserException

throwUserException :: String -> IO a
throwUserException = throwIO . UserException