{-# LANGUAGE OverloadedStrings #-}

module Traduisons.Util where

import Control.Monad.Except
import Data.List
import Data.Maybe
import Data.Time.Clock.POSIX
import Network.HTTP.Client as N
import Network.HTTP.Types
import Control.Exception
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Debug.Trace as Trace
import Traduisons.Types
import Traduisons.Resources

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

mkReq :: String -> Either TraduisonsError Request
mkReq url = let err = Left $ TErr CurlError ("Failed to parse URL: " ++ show url)
            in maybe err Right (parseUrl url)

curl :: (MonadIO m, Functor m) => URL -> StdMethod -> [Header] -> FormData
      -> Manager -> ExceptT TraduisonsError m B.ByteString
curl url httpMethod hdrs formData man = do
  req'' <- liftEither (mkReq url)
  req' <- case httpMethod of
            -- FIXME: Make a newtype for this
            GET -> return $ setQueryString formData req''
            POST -> let f (a, Just b) = (a, b)
                        f (a, Nothing) = (a, "")
                  in return $ urlEncodedBody (map f formData) req''
            _ -> let err = "Curl doesn't know " ++ show httpMethod
                 in throwError $ TErr CurlError err
  let ua = (hUserAgent, userAgent)
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

-- Remove the BOM from Unicode string
stripBOM :: String -> String
stripBOM s = fromMaybe s (stripPrefix "\65279" s)
