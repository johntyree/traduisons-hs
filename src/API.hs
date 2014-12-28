{-# LANGUAGE OverloadedStrings #-}

module API ( newState
           , detectLanguage
           , translate
           , runTraduisons
           , authorizedRequest
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
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import Resources
import Types
import Util


detectLanguage :: String -> Traduisons Language
detectLanguage s = do
  let url = "http://api.microsofttranslator.com/V2/Ajax.svc/Detect"
      urlData = [("text", s)]
  language <- authorizedRequest url urlData
  return $ Language language

translate :: Language -> Message -> Traduisons Message
translate targetLanguage message = do
  let url = "http://api.microsofttranslator.com/V2/Ajax.svc/Translate"
      fromLang = getLanguage . msgLanguage $ message
      toLang = getLanguage targetLanguage
      urlData = [ ("from", fromLang)
                , ("to", toLang)
                , ("text", msgBody message) ]
  translatedMessage <- authorizedRequest url urlData
  return $ Message targetLanguage translatedMessage

authorizedRequest :: URL -> [(B.ByteString, String)] -> Traduisons String
authorizedRequest url urlData' = do
  let wrap = Just . B.fromString
      headers token = [("Authorization", B.unwords ["Bearer", B.pack token])]
      urlData token = fmap wrap <$> ("appId", "Bearer " ++ token) : urlData'
  man <- liftIO $ N.newManager N.defaultManagerSettings
  tokenRef <- unTokenRef <$> ask
  token <- (trToken . tdToken) <$> liftIO (readIORef tokenRef)
  let hdrs = headers token
      formData = urlData token
      decodeAndStrip = read . stripBOM . B.toString
  result <- lift $ decodeAndStrip <$> curl url GET hdrs formData man
  checkExpired result

newState :: ErrorT String IO TraduisonsState
newState = do
  man <- liftIO $ N.newManager N.tlsManagerSettings
  clientSecret <- liftIO readClientSecret
  let formData = tokenRequestData clientSecret
  bytes <- curl tokenURL POST headers formData man
  now <- liftIO currentTime
  tokenResponse <- liftEither $ parseTokenResponse bytes
  let tokenData = TokenData (trExpiresIn tokenResponse + now) tokenResponse
  liftIO $ TokenRef <$> newIORef tokenData
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
