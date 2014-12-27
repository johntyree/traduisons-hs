{-# LANGUAGE OverloadedStrings #-}

module Types where

import Control.Monad.Error
import Control.Monad.Reader
import Control.Applicative
import Data.Aeson
import Data.ByteString.Char8
import Data.IORef

type Traduisons = ErrorT String (ReaderT TraduisonsState IO)
type TraduisonsState = IORef TokenData

-- runTraduisons r = either Left runReaderT . runErrorT
runTraduisons :: b -> ErrorT e (ReaderT b m) a -> m (Either e a)
runTraduisons r = flip runReaderT r . runErrorT

data TokenResponse = TokenResponse
  { trToken :: String
  , trScope :: String
  , trTokenType :: String
  , trExpiresIn :: Seconds
  } deriving (Show, Eq)

data TokenData = TokenData
  { tdExpiresAt :: Seconds
  , tdToken :: TokenResponse
  } deriving (Show, Eq)

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

mkMessage :: String -> a -> Message a
mkMessage = Message . Language

newtype Language = Language {getLanguage :: String}
  deriving (Show, Eq)

type Seconds = Integer

type FormData = [(ByteString, Maybe ByteString)]

type URL = String
