{-# LANGUAGE OverloadedStrings #-}

module Traduisons.Util where

import Control.Monad.Except
import Data.List
import Data.Maybe
import Data.Time.Clock.POSIX
import Network.HTTP.Client as N
import Network.HTTP.Types
import System.Process
import System.Exit
import Control.Exception
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Debug.Trace as Trace
import Traduisons.Types

trace :: Show a => String -> a -> a
trace s = join (Trace.trace . prefix s . show)
  where prefix :: String -> String -> String
        prefix "" a = a
        prefix p a = p ++ ": " ++ a

trace' :: Show a => a -> a
trace' = trace ""

liftEither :: (Monad m, MonadError e (t e m)) => Either e a -> t e m a
liftEither = either throwError return

currentTime :: IO Seconds
currentTime = fmap round getPOSIXTime

curl :: (MonadIO m, Functor m) => URL -> StdMethod -> [Header] -> FormData
      -> Manager -> ExceptT TraduisonsError m B.ByteString
curl url = if ssl then systemCurl url else nativeCurl url
  where ssl = "https" `isPrefixOf` url

mkReq :: String -> Either TraduisonsError Request
mkReq url = let err = Left $ TErr CurlError ("Failed to parse URL: " ++ show url)
            in maybe err Right (parseUrl url)

nativeCurl :: MonadIO m => URL -> StdMethod -> [Header] -> FormData
           -> Manager -> ExceptT TraduisonsError m B.ByteString
nativeCurl url httpMethod hdrs formData man = do
  req'' <- liftEither (mkReq url)
  req' <- case httpMethod of
            -- FIXME: Make a newtype for this
            GET -> return $ setQueryString formData req''
            POST -> let f (a, Just b) = (a, b)
                        f (a, Nothing) = (a, "")
                  in return $ urlEncodedBody (map f formData) req''
            _ -> let err = "Curl doesn't know " ++ show httpMethod
                 in throwError $ TErr CurlError err
  let ua = ("User-Agent", "traduisons/2.0.0")
      addHeaders h r = r { requestHeaders = ua:h ++ requestHeaders r }
      req = addHeaders hdrs req'
  -- WTF IS GOING ON HERE?
  let trySomeException = try $ httpLbs req man
      trySomeException :: IO (Either SomeException (Response BL.ByteString))
  resp <- lift . liftIO $ trySomeException
  return $ trace (show resp) ()
  case resp of
    Left err -> throwError $ TErr CurlError (show err)
    Right body -> return . BL.toStrict . responseBody $ body

-- Working around broken TLS (https://github.com/vincenthz/hs-tls/issues/87)
systemCurl :: (MonadIO m, Functor m) => URL -> StdMethod -> [Header]
           -> FormData -> Manager -> ExceptT TraduisonsError m B.ByteString
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
                   in throwError $ TErr CurlError err
  let args = "-s" : "-S" : url : map B.unpack (argHdrs ++ argData)
  curlResult <- liftIO (readProcessWithExitCode "curl" args "")
  case curlResult of
    (ExitSuccess, stdout, _) -> return (B.pack stdout)
    (ExitFailure _, _, stderr) -> throwError $ TErr CurlError stderr


-- Remove the BOM from Unicode string
stripBOM :: String -> String
stripBOM s = fromMaybe s (stripPrefix "\65279" s)
