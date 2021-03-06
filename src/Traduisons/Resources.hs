{-# LANGUAGE OverloadedStrings #-}

module Traduisons.Resources where

import Data.List
import System.Environment
import qualified Data.ByteString.Char8 as B

-- FIXME: Use file path combinators here

apiDomain :: String
apiDomain = "http://api.microsofttranslator.com"

apiPath :: String
apiPath =  "V2/Ajax.svc"

apiTranslateEndpoint :: String
apiTranslateEndpoint = "Translate"

apiDetectEndpoint :: String
apiDetectEndpoint = "Detect"

apiLanguageCodesEndpoint :: String
apiLanguageCodesEndpoint = "GetLanguagesForTranslate"

apiLanguageNamesEndpoint :: String
apiLanguageNamesEndpoint = "GetLanguageNames"

translationURL :: String
translationURL = intercalate "/" [apiDomain, apiPath, apiTranslateEndpoint]

detectionURL :: String
detectionURL = intercalate "/" [apiDomain, apiPath, apiDetectEndpoint]

languageCodeListURL :: String
languageCodeListURL = intercalate "/" [apiDomain, apiPath, apiLanguageCodesEndpoint]

languageNameListURL :: String
languageNameListURL = intercalate "/" [apiDomain, apiPath, apiLanguageNamesEndpoint]

tokenURL :: String
tokenURL = "https://datamarket.accesscontrol.windows.net/v2/OAuth2-13"

readClientSecret :: IO B.ByteString
readClientSecret = do
  secretEnv <- lookupEnv "TRADUISONS_SECRET"
  case secretEnv of
    Nothing -> B.readFile "SECRET"
    Just secret -> return (B.pack secret)

clientID :: B.ByteString
clientID = "Traduisons"

userAgent :: B.ByteString
userAgent = "traduisons/2.0.0"
