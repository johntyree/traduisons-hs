{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Applicative
import Network.HTTP.Types
import Network.HTTP.Client as N
import Network.HTTP.Client.TLS as N
import Data.Aeson
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import Types

translationDomain :: B.ByteString
translationDomain = "http://api.microsofttranslator.com"

translationURL :: String
translationURL = B.unpack translationDomain ++ "/V2/Ajax.svc/Translate"

tokenURL :: String
tokenURL = "https://datamarket.accesscontrol.windows.net/v2/OAuth2-13"
-- tokenURL = "https://httpbin.org/post"

clientSecret :: B.ByteString
clientSecret = ""

clientID :: B.ByteString
clientID = "Traduisons"

curl :: URL -> StdMethod -> [Header] -> FormData -> Manager
        -> IO (Either String B.ByteString)
curl url httpMethod hdrs formData man = do
  let maybeReq = fmap (addHeaders hdrs) . addPayload $ mkReq url
      ua = ("User-Agent", "traduisons/2.0.0")
      addHeaders h r = r { requestHeaders = ua:h ++ requestHeaders r }
      addPayload = case httpMethod of
                    GET -> fmap (setQueryString formData)
                    POST -> let f (a, Just b) = (a, b)
                                f (a, Nothing) = (a, "")
                            in fmap (urlEncodedBody (map f formData))
                    _ -> const (Left ("Curl doesn't know " ++ show httpMethod))
  case maybeReq of
    Left err -> return (Left err)
    Right req -> liftM (Right . BL.toStrict . responseBody) $ httpLbs req man

mkReq :: String -> Either String Request
mkReq url = let err = Left ("Failed to parse URL: " ++ show url)
            in maybe err Right (parseUrl url)

getToken :: IO (Either String TokenData)
getToken = do
  man <- N.newManager N.tlsManagerSettings
  bytes <- curl tokenURL POST headers tokenRequestData man
  now <- currentTime
  return $ bytes >>= tokenFromTokenResponse now
  where
    headers = [("User-Agent", "traduisons/2.0.0")]
    tokenRequestData = [
      ("client_id", Just clientID),
      ("client_secret", Just clientSecret),
      ("grant_type", Just "client_credentials"),
      ("scope", Just translationDomain)]

tokenFromTokenResponse :: Integer -> B.ByteString -> Either String TokenData
tokenFromTokenResponse now jsonBytes = fromJSONBytes $ BL.fromStrict jsonBytes
  where
    decodeErr = "Failed to decode: " ++ B.unpack jsonBytes
    toEither = maybe (Left decodeErr) Right
    tokenData tokenResp = TokenData (trExpiresIn tokenResp + now) tokenResp
    fromJSONBytes = toEither . fmap tokenData . decode

currentTime :: IO Seconds
currentTime = fmap round getPOSIXTime

detectLanguage :: String -> IO (Either String Language)
detectLanguage s = do
  man <- N.newManager N.defaultManagerSettings
  eitherToken <- getToken
  result <- case eitherToken of
    Left err -> return $ Left err
    Right td ->
      let token = trToken . tdToken $ td
          url = "http://api.microsofttranslator.com/V2/Ajax.svc/Detect"
          urlData = [ ("appId", Just (B.unwords ["Bearer", B.pack token]))
                    , ("text", (Just . urlEncode True . B.pack) s) ]
          headers = []
      in curl url GET headers urlData man
  return $ (Language . stripBOM . B.unpack) <$> result

stripBOM :: String -> String
stripBOM ('\239':'\187':'\191':txt) = txt
stripBOM txt = txt

mkMessage :: String -> a -> Message a
mkMessage lang = Message (Language lang)

            -- var s = document.createElement("script");
            -- s.src = "http://api.microsofttranslator.com/V2/Ajax.svc/Translate" +
                -- "?appId=Bearer " + encodeURIComponent(window.accessToken) +
                -- "&from=" + encodeURIComponent(from) +
                -- "&to=" + encodeURIComponent(to) +
                -- "&text=" + encodeURIComponent(text) +
                -- "&oncomplete=mycallback";
            -- document.body.appendChild(s);
