module Main where

import Data.Aeson (ToJSON(..))

import Redsift.CGI
import Redsift.DB

main = spawnSCGI "127.0.0.1" "9000" dispatch

dispatch "GET" ["table", "list"] = toResponse (toJSON `fmap` allTables)
dispatch "GET" ["query"] = do
  q <- queryVarRequired "q"
  toResponse (toJSON `fmap` query q 100)
dispatch _ _ = return notFound
