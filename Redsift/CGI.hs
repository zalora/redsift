{-# LANGUAGE FlexibleInstances #-}

-- This is a bunch of code that I haven't put into the sscgi library
-- because I'm not satisfied with the interface yet.

module Redsift.CGI ( SCGI, SCGIT, Response, Resp(..)
                   , notFound, notAuthorized, notAllowed, emptyResponse
                   , queryVars, queryVar, queryVarDefault, queryVarRequired
                   , bodyVars, bodyVar, bodyVarDefault, bodyVarRequired, bodyJSON
                   , redirect, permRedirect
                   , spawnSCGI
                   ) where

import Control.Arrow (second)
import Control.Concurrent (forkIO)
import Control.Exception (finally)
import Control.Monad (forever)
import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)
import Data.Aeson (Value(..), FromJSON(..), decode, encode)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.List (find, intercalate)
import Data.List.Split (splitOn)
import Data.List.Utils as LU -- MissingH
import Data.Maybe (fromMaybe)
import Network (accept)
import Network.Socket (getAddrInfo, addrAddress, listen, socket, Family(..), SocketType(..), defaultProtocol, bindSocket, SocketOption(..), setSocketOption)
import Network.CGI (formDecode)
import qualified Network.SCGI as SCGI
import Network.SCGI (Response(..), SCGIT)
import Network.URI (unEscapeString)
import System.IO (hClose)

type SCGI = SCGIT IO

notFound :: Response
notFound = Response "404 Resource Not Found" ""

notAuthorized :: Response
notAuthorized = Response "401 Unauthorized" ""

notAllowed :: Response
notAllowed = Response "504 Method not allowed" ""

emptyResponse :: Response
emptyResponse = Response "200 OK" ""

class Resp a where
  toResponse :: a -> SCGI Response

instance Resp String where
  toResponse string = do
    SCGI.setHeader "Content-Type" "text/plain; charset=utf-8"
    return $ Response "200 OK" $ BLU.fromString string

instance Resp Value where
  toResponse obj = do
    SCGI.setHeader "Content-Type" "application/json"
    return $ Response "200 OK" $ encode obj

instance (Resp a) => Resp (Maybe a) where
  toResponse Nothing = return notFound
  toResponse (Just r) = toResponse r

instance (Resp a) => Resp (IO a) where
  toResponse a = toResponse =<< liftIO a

instance Resp (SCGI Response) where
  toResponse = id

queryVars :: Monad m => SCGIT m [(String, String)]
queryVars = do
  queryString <- SCGI.header "QUERY_STRING"
  return $ maybe [] (map (second convertLineEndings) . formDecode . BU.toString) queryString

queryVar :: Monad m => String -> SCGIT m (Maybe String)
queryVar name = lookup name `liftM` queryVars

queryVarDefault :: Monad m => String -> String -> SCGIT m String
queryVarDefault name def' = fromMaybe def' `liftM` queryVar name

queryVarRequired :: Monad m => String -> SCGIT m String
queryVarRequired name = queryVarDefault name (error $ "Parameter '" ++ name ++ "' is required.")

bodyVars :: Monad m => SCGIT m [(String, String)]
bodyVars = (map (second convertLineEndings) . formDecode . BLU.toString) `liftM` SCGI.body

bodyVar :: Monad m => String -> SCGIT m (Maybe String)
bodyVar name = lookup name `liftM` bodyVars

bodyVarDefault :: Monad m => String -> String -> SCGIT m String
bodyVarDefault name def' = fromMaybe def' `liftM` bodyVar name

bodyVarRequired :: Monad m => String -> SCGIT m String
bodyVarRequired name = bodyVarDefault name (error $ "Parameter '" ++ name ++ "' is required.")

bodyJSON :: (Monad m, FromJSON a) => SCGIT m (Maybe a)
bodyJSON = decode `liftM` SCGI.body

redirect :: String -> SCGI Response
redirect url = do
  SCGI.setHeader "Location" (BU.fromString url)
  return $ Response "302 Found" $ BLU.fromString $ "Redirecting you to: " ++ url

permRedirect :: String -> SCGI Response
permRedirect url = do
  SCGI.setHeader "Location" (BU.fromString url)
  return $ Response "301 Moved Permanently" ""

-- We might get data from various sources that use different end-of-line
-- terminators. But we want to always work with just newlines.

-- We use |join| instead of |unlines| because |unlines| adds a trailing newline.

convertLineEndings :: String -> String
convertLineEndings = LU.join "\n" . splitLines

-- This comes from Real World Haskell.

splitLines :: String -> [String]
splitLines [] = []
splitLines cs =
  let (pre, suf) = break isLineTerminator cs in
  pre : case suf of
          ('\r':'\n':rest) -> splitLines rest
          ('\r':rest)      -> splitLines rest
          ('\n':rest)      -> splitLines rest
          _                -> []

isLineTerminator :: Char -> Bool
isLineTerminator = (`elem` "\r\n")

spawnSCGI :: String -> String -> (BU.ByteString -> [String] -> SCGI Response) -> IO ()
spawnSCGI addr port dispatcher = do
  s <- listen'
  forever $ do
    (handle, _, _) <- accept s
    _ <- forkIO $ finally (SCGI.runRequest handle (handleRequest dispatcher))
                          (hClose handle)
    return ()
 where listen' = do
         addrs <- getAddrInfo Nothing (Just addr) (Just port)
         s <- socket AF_INET Stream defaultProtocol
         setSocketOption s ReuseAddr 1
         bindSocket s $ addrAddress $ head addrs
         listen s 5
         return s

handleRequest :: (BU.ByteString -> [String] -> SCGI Response) -> SCGI Response
handleRequest dispatcher = do
  method' <- SCGI.method
  path' <- SCGI.path
  case (method', path') of
    (Just method, Just path) -> (dispatch dispatcher) method (pathList $ BU.toString path)
    _ -> error "Missing request method or path."

-- Before we actually dispatch the request, we use the opportunity to clean up
-- the URI and redirect the client if necessary. This handles cases like
-- trailing slashes. We want only one URI to point to a resource.

dispatch :: (BU.ByteString -> [String] -> SCGI Response) -> BU.ByteString -> [String] -> SCGI Response
dispatch dispatcher meth path =
  case path of
    ["",""] -> toResponse ("Front Page"::String) -- toResponse $ frontPage -- "/"
    ("":xs) -> case find (== "") xs of
                 Nothing -> dispatcher meth xs
                 Just _  -> redirect $ '/' : intercalate "/" (filter (/= "") xs)
    _       -> return notFound

-- We extract the path part of the URI, ``unescape it'' (convert % codes back
-- to characters), decode it (convert UTF-8 characters to Unicode Chars), and
-- finally parse it into directory and filename components. For example,

-- /some/directory/and/a/filename

-- becomes

-- ["some","directory","and","a","filename"]

-- Note that the parser does not have to deal with query strings or fragments
-- because |uriPath| has already stripped them.

-- The one case this doesn't handle correctly is @//something@, because it's
-- handled differently by |Network.CGI|.

pathList :: String -> [String]
pathList = splitOn "/" . unEscapeString
