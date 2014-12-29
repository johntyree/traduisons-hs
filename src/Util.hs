{-# LANGUAGE OverloadedStrings #-}

module Util where

import Control.Applicative
import Control.Monad.Error
import Data.List
import Data.Maybe
import Data.Time.Clock.POSIX
import Network.HTTP.Client as N
import Network.HTTP.Types
import System.Process
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import Types

liftEither :: (Error e, Monad m, MonadError e (t e m)) => Either e a -> t e m a
liftEither = either throwError return

currentTime :: IO Seconds
currentTime = fmap round getPOSIXTime

curl :: (MonadIO m, Functor m) => URL -> StdMethod -> [Header] -> FormData
      -> Manager -> ErrorT String m B.ByteString
curl url = if ssl then systemCurl url else nativeCurl url
  where ssl = "https" `isPrefixOf` url

mkReq :: String -> Either String Request
mkReq url = let err = Left ("Failed to parse URL: " ++ show url)
            in maybe err Right (parseUrl url)

nativeCurl :: MonadIO m => URL -> StdMethod -> [Header] -> FormData
           -> Manager -> ErrorT String m B.ByteString
nativeCurl url httpMethod hdrs formData man = do
  req'' <- liftEither (mkReq url)
  req' <- case httpMethod of
            GET -> return $ setQueryString formData req''
            POST -> let f (a, Just b) = (a, b)
                        f (a, Nothing) = (a, "")
                  in return $ urlEncodedBody (map f formData) req''
            _ -> let err = "Curl doesn't know " ++ show httpMethod
                 in throwError err
  let ua = ("User-Agent", "traduisons/2.0.0")
      addHeaders h r = r { requestHeaders = ua:h ++ requestHeaders r }
      req = addHeaders hdrs req'
  resp <- liftIO $ httpLbs req man
  return . BL.toStrict . responseBody $ resp

-- Working around broken TLS (https://github.com/vincenthz/hs-tls/issues/87)
systemCurl :: (MonadIO m, Functor m) => URL -> StdMethod -> [Header]
           -> FormData -> Manager -> ErrorT String m B.ByteString
systemCurl url httpMethod hdrs formData _ = do
  let dataAsArg = [B.concat [a, "=", b] | (a, Just b) <- formData]
      addFlag flag argList = flag : intersperse flag argList
      -- It would be nicer here to use something to just drop the case
      -- insensistivity and keep the ByteString, but whatever
      hdr (a, b) = B.concat [read (show a), ":", b]
      argHdrs = addFlag "-H" $ map hdr hdrs
  argData <- case httpMethod of
              GET -> return $ "-G" : addFlag "-d" dataAsArg
              POST -> return $ addFlag "--data-urlencode" dataAsArg
              _ -> let err = "Curl doesn't know " ++ show httpMethod
                   in throwError err
  let args = "-s" : url : map B.unpack (argHdrs ++ argData)
  B.pack <$> liftIO (readProcess "curl" args "")

-- Remove the BOM from Unicode string
stripBOM :: String -> String
stripBOM s = fromMaybe s (stripPrefix "\65279" s)
