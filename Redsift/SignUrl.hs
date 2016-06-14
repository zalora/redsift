{-# language OverloadedStrings, NamedFieldPuns #-}

module Redsift.SignUrl (signUrl, URI) where


import Network.HTTP
import Network.AWS.Authentication
import Network.AWS.AWSConnection
import Network.URI
import System.FilePath


type Bucket = String
type Object = String

-- $ Signs a url. Outputs the uri with the bucket as subdomain (not subpath).
signUrl :: (String, String) -> Bucket -> Object -> Integer -> URI
signUrl (accessKey, secretKey) bucket object expirationTime =
    convertToEndpointURI (preSignedURI action expirationTime)
  where
    action :: S3Action
    action = S3Action {
        s3conn = connection,
        s3bucket = bucket,
        s3object = object,
        s3query = "",
        s3metadata = [],
        s3body = "",
        s3operation = GET
      }
    connection :: AWSConnection
    connection = AWSConnection {
        awsHost = "s3.amazonaws.com",
        awsPort = 80,
        awsAccessKey = accessKey,
        awsSecretKey = secretKey
      }


-- | Network.AWS.Authentication.preSignedURI does return the signed URI
-- with the bucket as a subpath, but we need it as a subdomain.
-- (I think that buckets as subdomains are called endpoints.)
convertToEndpointURI :: URI -> URI
convertToEndpointURI input = case input of
    URI{uriAuthority = Just (URIAuth userInfo regName _), uriPath} ->
        case splitDirectories uriPath of
            ("/" : bucket : rest) ->
                input{
                    uriScheme = "https:",
                    uriAuthority = Just $ URIAuth userInfo
                        (bucket ++ "." ++ regName) "",
                    uriPath = joinPath ("/" : rest)
                }
            _ -> error ("convertToEndpointURI: uri should start with /$BUCKET: " ++ show input)
    _ ->
        error ("convertToEndpointURI: uri should contain the authority: " ++ show input)
