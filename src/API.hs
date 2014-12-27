{-# LANGUAGE OverloadedStrings #-}

module API ( getToken
           , newState
           , detectLanguage
           , translate
           , runTraduisons
           , execTraduisons
           ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Data.Aeson
import Data.IORef
import Data.List
import Network.HTTP.Client as N
import Network.HTTP.Client.TLS as N
import Network.HTTP.Types
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import Resources
import Types
import Util


execTraduisons :: Traduisons a -> IO (Either String a)
execTraduisons action = newState >>= either reraise run
  where
    reraise = return . throwError
    run = (`runTraduisons` action)

detectLanguage :: String -> Traduisons Language
detectLanguage s = do
  let url = "http://api.microsofttranslator.com/V2/Ajax.svc/Detect"
      urlData = [("text", s)]
  language <- traduisonsRequest url urlData
  return $ Language language

translate :: Language -> Message String -> Traduisons (Message String)
translate targetLanguage message = do
  let url = "http://api.microsofttranslator.com/V2/Ajax.svc/Translate"
      fromLang = getLanguage . msgLanguage $ message
      toLang = getLanguage targetLanguage
      urlData = [ ("from", fromLang)
                , ("to", toLang)
                , ("text", msgBody message) ]
  translatedMessage <- traduisonsRequest url urlData
  return $ Message targetLanguage translatedMessage

traduisonsRequest :: URL -> [(B.ByteString, String)] -> Traduisons String
traduisonsRequest url urlData' = do
  let wrap = Just . B.pack
      headers token = [("Authorization", B.unwords ["Bearer", B.pack token])]
      urlData = ("appId", Nothing) : map (fmap wrap) urlData'
  man <- liftIO $ N.newManager N.defaultManagerSettings
  tokenRef <- lift ask
  token <- (trToken . tdToken) <$> liftIO (readIORef tokenRef)
  result <- strip . B.unpack <$> curl url GET (headers token) urlData man
  checkExpired result

newState :: IO (Either String TraduisonsState)
newState = runTraduisons undefined getToken

getToken :: Traduisons TraduisonsState
getToken = do
  man <- liftIO $ N.newManager N.tlsManagerSettings
  clientSecret <- liftIO readClientSecret
  bytes <- curl tokenURL POST headers (tokenRequestData clientSecret) man
  tokR <- liftEither (parseTokenResponse bytes)
  now <- liftIO currentTime
  liftIO . newIORef $ TokenData (trExpiresIn tokR + now) tokR
  where
    headers = [("User-Agent", "traduisons/2.0.0")]
    tokenRequestData cs = [
      ("client_id", Just clientID),
      ("client_secret", Just cs),
      ("grant_type", Just "client_credentials"),
      ("scope", Just translationDomain)]

parseTokenResponse :: B.ByteString -> Either String TokenResponse
parseTokenResponse jsonBytes = fromJSONBytes $ BL.fromStrict jsonBytes
  where
    decodeErr = "Failed to decode: " ++ B.unpack jsonBytes
    toEither = maybe (Left decodeErr) Right
    fromJSONBytes = toEither . decode

checkExpired :: String -> Traduisons String
checkExpired s = if "ArgumentException:" `isPrefixOf` s
                 then throwError s
                 else return s

strip :: String -> String
strip ('\239':'\187':'\191':txt) = read txt
strip ('\255':txt) = read txt
strip txt = txt
