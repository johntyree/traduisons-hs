{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Traduisons.Types where

import Control.Monad.Error
import Control.Monad.Reader
import Control.Applicative
import Data.Aeson
import Data.ByteString.Char8
import Data.IORef

newtype Traduisons a = Traduisons {
  unTraduisons :: ReaderT TraduisonsState (ErrorT String IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader TraduisonsState,
            MonadError String)

type TraduisonsState = TokenRef
newtype TokenRef = TokenRef { unTokenRef :: IORef TokenData }

liftErrorT :: ErrorT String IO a -> Traduisons a
liftErrorT = Traduisons . lift

runTraduisons :: TraduisonsState -> Traduisons a -> IO (Either String a)
runTraduisons = flip $ (runErrorT .) . runReaderT . unTraduisons

instance Show TokenRef where
  show = const "<TokenRef: API token reference>"

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

data Message = Message {msgLanguage :: Language, msgBody :: String}
  deriving (Show, Eq)

mkMessage :: String -> String -> Message
mkMessage = Message . Language

newtype Language = Language {getLanguage :: String}
  deriving (Show, Eq)

type Seconds = Integer

type FormData = [(ByteString, Maybe ByteString)]

type URL = String

data AppState = AppState
  { asFromLang :: Language
  , asToLang :: Language
  , asHistory :: [Command]
  , asTraduisonsState :: TraduisonsState
  } deriving Show

instance Eq AppState where
  AppState f l _ _ == AppState f' l' _ _ = f == f' && l == l'

data Command = SetFromLanguage String
             | SetToLanguage String
             | Translate String
             | SwapLanguages
             | Exit
  deriving (Show, Eq)
