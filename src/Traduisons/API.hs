{-# LANGUAGE OverloadedStrings #-}

module Traduisons.API ( newState
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
import Data.List.Split
import Data.Maybe
import Text.Read
import Network.HTTP.Client as N
import Network.HTTP.Client.TLS as N
import Network.HTTP.Types
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import Traduisons.Resources
import Traduisons.Types
import Traduisons.Util


detectLanguage :: String -> Traduisons Language
detectLanguage s = do
  let url = detectionURL
      urlData = [("text", s)]
  language <- authorizedRequest url urlData
  return $ Language language

translate :: Language -> Message -> Traduisons Message
translate targetLanguage message = do
  let url = translationURL
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
  let hdrs           = headers token
      formData       = urlData token
  curlResult <- liftErrorT $ B.toString <$> curl url GET hdrs formData man
  let decodedAndStripped = convertStupidUnicodeNewline . stripBOM $ curlResult
      defaultError = Left $ TErr NoStringError decodedAndStripped
      maybeResult = Right <$> readMaybe decodedAndStripped
      deserialized = fromMaybe defaultError maybeResult
  result <- liftErrorT . liftEither $ deserialized
  case checkException result of
    Nothing -> return result
    Just err -> throwError err

newState :: ErrorT TraduisonsError IO TraduisonsState
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
      ("scope", Just $ B.pack apiDomain)]

parseTokenResponse :: B.ByteString -> Either TraduisonsError TokenResponse
parseTokenResponse jsonBytes = fromJSONBytes $ BL.fromStrict jsonBytes
  where
    decodeErr = TErr UnrecognizedJSONError errMsg
    errMsg =  "Failed to decode: " ++ B.unpack jsonBytes
    toEither = maybe (Left decodeErr) Right
    fromJSONBytes = toEither . decode

convertStupidUnicodeNewline :: String -> String
convertStupidUnicodeNewline = intercalate "\n" . splitOn "\\u000d\\u000a"

checkException :: String -> Maybe TraduisonsError
checkException s = do
  let (header, m) = break (== ':') s
      flags = [ArgumentOutOfRangeException .. UnknownError]
  flip TErr m <$> find (flip isInfixOf header . show) flags
