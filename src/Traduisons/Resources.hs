{-# LANGUAGE OverloadedStrings #-}

module Traduisons.Resources where

import Data.List
import qualified Data.ByteString.Char8 as B

apiDomain :: String
apiDomain = "http://api.microsofttranslator.com"

apiPath :: String
apiPath =  "V2/Ajax.svc"

apiTranslateEndpoint :: String
apiTranslateEndpoint = "Translate"

apiDetectEndpoint :: String
apiDetectEndpoint = "Detect"

translationURL :: String
translationURL = intercalate "/" [apiDomain, apiPath, apiTranslateEndpoint]

detectionURL :: String
detectionURL = intercalate "/" [apiDomain, apiPath, apiDetectEndpoint]

tokenURL :: String
tokenURL = "https://datamarket.accesscontrol.windows.net/v2/OAuth2-13"

readClientSecret :: IO B.ByteString
readClientSecret = B.readFile "SECRET"

clientID :: B.ByteString
clientID = "Traduisons"
