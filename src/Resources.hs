{-# LANGUAGE OverloadedStrings #-}

module Resources where

import qualified Data.ByteString.Char8 as B

translationDomain :: B.ByteString
translationDomain = "http://api.microsofttranslator.com"

translationURL :: String
translationURL = B.unpack translationDomain ++ "/V2/Ajax.svc/Translate"

tokenURL :: String
tokenURL = "https://datamarket.accesscontrol.windows.net/v2/OAuth2-13"

readClientSecret :: IO B.ByteString
readClientSecret = B.readFile "SECRET"

clientID :: B.ByteString
clientID = "Traduisons"
