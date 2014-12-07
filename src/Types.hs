{-# LANGUAGE OverloadedStrings #-}

module Types where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.ByteString.Char8

data MyMemoryResponse = MyMemoryResponse
  { mmrReponseCode :: Integer
  , mmrTranslation :: String
  }
  deriving (Show, Eq)

data TokenResponse = TokenResponse
  { trToken :: String
  , trScope :: String
  , trTokenType :: String
  , trExpiresIn :: Seconds
  } deriving (Show, Eq)

data TokenData = TokenData
  { trExpiresAt :: Seconds
  , tdToken :: TokenResponse
  } deriving (Show, Eq)

instance FromJSON MyMemoryResponse where
  parseJSON (Object o) = do
    let (.:>) p key = p >>= (.: key)
    stat <- o .: "responseStatus"
    trans <- o .: "responseData" .:> "translatedText"
    return $ MyMemoryResponse stat trans
  parseJSON _ = mzero

instance FromJSON TokenResponse where
  parseJSON (Object o) = do
    token <- o .: "access_token"
    scope <- o .: "scope"
    tokenType <- o .: "token_type"
    expiry <- read <$> o .: "expires_in"
    return $ TokenResponse token scope tokenType expiry
  parseJSON _ = mzero

data Message a = Message {msgLanguage :: Language, msgBody :: a}
  deriving (Show, Eq)

newtype Language = Language {getLanguage :: String}
  deriving (Show, Eq)

type Seconds = Integer

type FormData = [(ByteString, Maybe ByteString)]

type URL = String
