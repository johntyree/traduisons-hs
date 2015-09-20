{-# LANGUAGE OverloadedStrings #-}

module Traduisons.API ( authorizedRequest
                      , detectLanguage
                      , getLanguagesForTranslate
                      , mkTraduisonsState
                      , newState
                      , renewToken
                      , runTraduisons
                      , translate
                      ) where

import Control.Concurrent.MVar
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Text.Read as T
import Network.HTTP.Client as N
import Network.HTTP.Client.TLS as N
import Network.HTTP.Types
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import Traduisons.Resources
import Traduisons.Types
import Traduisons.Util


getLanguagesForTranslate :: Traduisons [String]
getLanguagesForTranslate = authorizedRequest languageListURL [] >>= validateResponse

-- | Detect the language of a body of text
detectLanguage :: String -> Traduisons Language
detectLanguage s = do
  let url = detectionURL
      urlData = [("text", s)]
  language <- authorizedRequest url urlData >>= validateResponse
  return $ Language language

-- | Given a target 'Language', convert a 'Message' to that language
translate :: Language -> Message -> Traduisons Message
translate targetLanguage message = do
  let url = translationURL
      fromLang = getLanguage . msgLanguage $ message
      toLang = getLanguage targetLanguage
      urlData = [ ("from", fromLang)
                , ("to", toLang)
                , ("text", msgBody message) ]
  translatedMessage <- authorizedRequest url urlData >>= validateResponse
  return $ Message targetLanguage translatedMessage

-- | Build a query, additionally inserting necessary authorization credentials,
-- and retrieve it, stripping the BOM and returning the result as a
-- ByteString
authorizedRequest :: URL -> [(B.ByteString, String)] -> Traduisons B.ByteString
authorizedRequest url urlData' = do
  let wrap = Just . B.fromString
      headers token = [("Authorization", B.unwords ["Bearer", B.pack token])]
      urlData token = fmap wrap <$> ("appId", "Bearer " ++ token) : urlData'
  man <- liftIO $ N.newManager N.defaultManagerSettings
  tokenRef <- unTokenRef <$> ask
  token <- (trToken . tdToken) <$> liftIO (readMVar tokenRef)
  let hdrs           = headers token
      formData       = urlData token
      strip = B.fromString . convertStupidUnicodeNewline . stripBOM . B.toString
  strip <$> liftExceptT (curl url GET hdrs formData man)

-- | Given a raw response such as from authorizeRequest, attempt to decode it
-- into the appropriate type. This uses 'readMaybe' rather than
-- 'Data.Aeson.decode' because a plain String type is not valid JSON and
-- always return Nothing.
validateResponse :: (Show a, Read a) => B.ByteString -> Traduisons a
validateResponse response = do
  let s = B.toString response
      defaultError = Left $ TErr UnrecognizedJSONError s
      maybeResult = Right <$> T.readMaybe s
      deserialized = fromMaybe defaultError maybeResult
  result <- liftExceptT . liftEither $ deserialized
  case checkException s of
    Nothing -> return result
    Just err -> throwError $ trace "Validation error" err

-- | Stuff a fresh auth token into the current 'AppState'
renewToken :: StateT AppState (ExceptT TraduisonsError IO) ()
renewToken = do
  appState <- get
  let traduisonsState = asTraduisonsState appState
  tokenData <- lift newState
  liftIO $ putMVar (unTokenRef traduisonsState) tokenData

-- | Retrieve an auth token
newState :: ExceptT TraduisonsError IO TokenData
newState = do
  man <- liftIO $ N.newManager N.tlsManagerSettings
  clientSecret <- liftIO readClientSecret
  let formData = tokenRequestData clientSecret
  bytes <- curl tokenURL POST headers formData man
  now <- liftIO currentTime
  tokenResponse <- liftEither $ parseTokenResponse bytes
  return $ TokenData (trExpiresIn tokenResponse + now) tokenResponse
  where
    -- FIXME: pull header name out of Network...Types?
    headers = [("User-Agent", "traduisons/2.0.0")]
    tokenRequestData cs = [
      ("client_id", Just clientID),
      ("client_secret", Just cs),
      ("grant_type", Just "client_credentials"),
      ("scope", Just $ B.pack apiDomain)]

mkTraduisonsState :: IO TraduisonsState
mkTraduisonsState = TokenRef <$> newEmptyMVar

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
