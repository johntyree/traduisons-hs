{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Traduisons.Types where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Concurrent.MVar
import Data.Aeson
import Data.ByteString.Char8
import qualified Data.Map as M

-- | The 'Traduisons' Monad gives you a stateful way to interact with the
-- translation API.
newtype Traduisons a = Traduisons {
  unTraduisons :: ReaderT TraduisonsState (ExceptT TraduisonsError IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader TraduisonsState,
            MonadError TraduisonsError)
-- this is good

-- | The failure modes and their descriptions.
data TraduisonsError = TErr TraduisonsErrorFlag String
  deriving (Show, Eq)

-- | 'TraduisionsState' is an authentication token acquired from
-- the translation API. It's stored in an MVar as it will need to be updated
-- regularly.
type TraduisonsState = TokenRef
newtype TokenRef = TokenRef { unTokenRef :: MVar TokenData }

liftExceptT :: ExceptT TraduisonsError IO a -> Traduisons a
liftExceptT = Traduisons . lift

-- | Runs a traduisons action
runTraduisons :: TraduisonsState -> Traduisons a
              -> IO (Either TraduisonsError a)
runTraduisons = flip $ (runExceptT .) . runReaderT . unTraduisons

instance Show TokenRef where
  show = const "<TokenRef: API token reference>"

data TokenResponse = TokenResponse
  { trToken :: String -- ^ The auth token
  , trScope :: String -- ^ The scope of the auth token. This limits capabilities
  , trTokenType :: String
  , trExpiresIn :: Seconds  -- ^ The number of seconds until the token expires
  } deriving (Show, Eq)

emptyTokenResponse :: TokenResponse
emptyTokenResponse = TokenResponse "" "" "" 0

data TokenData = TokenData
  { tdExpiresAt :: Seconds -- ^ The token's expiration time in epoch seconds
  , tdToken :: TokenResponse -- ^ The auth token data
  } deriving (Show, Eq)

emptyTokenData :: TokenData
emptyTokenData = TokenData 0 emptyTokenResponse

instance FromJSON TokenResponse where
  parseJSON (Object o) = do
    token <- o .: "access_token"
    scope <- o .: "scope"
    tokenType <- o .: "token_type"
    expiry <- read <$> o .: "expires_in"
    return $ TokenResponse token scope tokenType expiry
  parseJSON _ = mzero

-- | A 'Message' represents a body of text in a known language
data Message = Message {msgLanguage :: Language, msgBody :: String}
  deriving (Show, Eq)

mkMessage :: String -> String -> Message
mkMessage = Message . Language

newtype Language = Language {getLanguage :: String}
  deriving (Show, Eq)

type Seconds = Integer

type FormData = [(ByteString, Maybe ByteString)]

type URL = String

-- | The application level state, independent of both the lower level
-- translation machinery and the higher level UI state
data AppState = AppState
  { asFromLang :: Language -- ^ The current input language
  , asToLang :: Language -- ^ The target output language
  , asHistory :: [(Command, Maybe Message)] -- ^ The collection of all inputs
                                            -- and their translations thus far.
  , asLanguageNameCodes :: M.Map String String
  , asTraduisonsState :: TraduisonsState -- ^ The underlying translator's state
  } deriving Show

instance Eq AppState where
  AppState f l h _ _ == AppState f' l' h' _ _ = f == f' && l == l' && h == h'

-- | Operations that transform the 'AppState' in some way
data Command = SetFromLanguage String
             | SetToLanguage String
             | Translate String
             | DetectLanguage String
             | SwapLanguages
             | Help
             | Exit
  deriving (Show, Eq)

{- Microsoft does not appear to have documented the possible exceptions that
   the translator API might return, and they change them unannounced. -}
-- FIXME: We can get much better error handling if we translate an array of
-- text. See https://msdn.microsoft.com/en-us/library/ff512407.aspx
-- Also consider retrieiving multiple translations.
-- https://msdn.microsoft.com/en-us/library/ff512403.aspx
-- | Anticipated error modes
data TraduisonsErrorFlag = ArgumentOutOfRangeException
                         | CurlError
                         | TraduisonsExit
                         | TraduisonsHelp
                         | ArgumentException
                         | LanguageDetectionError
                         | UnknownError
                         | UnrecognizedJSONError
  deriving (Show, Eq, Enum)
