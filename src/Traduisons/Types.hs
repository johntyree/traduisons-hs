{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Traduisons.Types where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Concurrent.MVar
import Data.Aeson
import Data.ByteString.Char8

newtype Traduisons a = Traduisons {
  unTraduisons :: ReaderT TraduisonsState (ExceptT TraduisonsError IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader TraduisonsState,
            MonadError TraduisonsError)

data TraduisonsError = TErr TraduisonsErrorFlag String
  deriving (Show, Eq)

type TraduisonsState = TokenRef
newtype TokenRef = TokenRef { unTokenRef :: MVar TokenData }

liftErrorT :: ExceptT TraduisonsError IO a -> Traduisons a
liftErrorT = Traduisons . lift

runTraduisons :: TraduisonsState -> Traduisons a
              -> IO (Either TraduisonsError a)
runTraduisons = flip $ (runExceptT .) . runReaderT . unTraduisons

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
  , asHistory :: [(Command, Maybe Message)]
  , asTraduisonsState :: TraduisonsState
  } deriving Show

instance Eq AppState where
  AppState f l h _ == AppState f' l' h' _ = f == f' && l == l' && h == h'

data Command = SetFromLanguage String
             | SetToLanguage String
             | Translate String
             | DetectLanguage String
             | SwapLanguages
             | Exit
  deriving (Show, Eq)

{- Microsoft does not appear to have documented the possible exceptions that
   the translator API might return, and they change them unannounced. -}
data TraduisonsErrorFlag = ArgumentOutOfRangeException
                         | CurlError
                         | NoStringError
                         | TraduisonsExit
                         | ArgumentException
                         | LanguageDetectionError
                         | UnknownError
                         | UnrecognizedJSONError
  deriving (Show, Eq, Enum)
