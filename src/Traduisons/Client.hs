
module Traduisons.Client where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Data.List
import Data.List.Split

import Traduisons.API
import Traduisons.Types

helpMsg :: String
helpMsg = "Help yourself."

runTest :: String -> IO (Either TraduisonsError (Maybe Message, AppState))
runTest input = do
  let commands = concatMap parseInput $ splitOn ";" input
      action st = runCommands (Just st) commands
  createAppState >>= action

createAppState :: IO AppState
createAppState =
  let new = return . AppState (Language "auto") (Language "en") []
  in runErrorT newState >>= either (error . show) new

renewToken :: StateT AppState (ErrorT TraduisonsError IO) (Maybe a)
renewToken = do
  appState <- get
  traduisonsState <- lift newState
  put appState { asTraduisonsState = traduisonsState }
  return Nothing

parseInput :: String -> [Command]
parseInput ('/':s) = SwapLanguages : parseInput s
parseInput "\EOT"  = [Exit]
parseInput ""      = []
parseInput ('|':s) = [SetToLanguage s]
parseInput s
  | "/" `isSuffixOf` s = SwapLanguages : parseInput (init s)
  | "|" `isInfixOf` s = let (from, '|':to) = break (== '|') s
                            f ctor l = [ctor l | not (null l)]
                        in f SetFromLanguage from ++ f SetToLanguage to
  | otherwise = [Translate s]

runCommands :: Maybe AppState -> [Command]
            -> IO (Either TraduisonsError (Maybe Message, AppState))
runCommands appState cmds = do
  let app = foldM (const runCommand) Nothing cmds
  initial <- maybe (liftIO createAppState) return appState
  runErrorT . runStateT app $ initial

runCommand :: Command -> StateT AppState (ErrorT TraduisonsError IO) (Maybe Message)
runCommand c = do
  msg <- runCommand' c
  modify (\a -> a { asHistory = (c, msg) : asHistory a })
  return msg
runCommand' :: Command -> StateT AppState (ErrorT TraduisonsError IO) (Maybe Message)
runCommand' SwapLanguages = get >>= \aS -> from aS >> to aS
  where from = runCommand' . SetFromLanguage . getLanguage . asToLang
        to = runCommand' . SetToLanguage . getLanguage . asFromLang
runCommand' (SetFromLanguage l) = const Nothing <$> modify setFromLang
  where setFromLang appState = appState { asFromLang = Language l }
runCommand' (SetToLanguage l) = const Nothing <$> modify setToLang
  where setToLang appState = appState { asToLang = Language l }
runCommand' (Translate rawMsg) = go (1 :: Int)
  where
    go n = do
      AppState fromLang' _ _ _ <- get
      when (getLanguage fromLang' == "auto") $ void $ runCommand' (DetectLanguage rawMsg)
      AppState fromLang toLang _ tradState <- get
      let message = Message fromLang rawMsg
      translation <- liftIO . runTraduisons tradState $ translate toLang message
      case translation of
        Left err ->
          if n > 0
          then renewToken >> go 0
          else throwError err
        Right msg -> return $ Just msg

runCommand' (DetectLanguage rawMsg) = do
  result <- withTokenRefresh (detectLanguage rawMsg)
  case result of
    Nothing -> throwError $ TErr LanguageDetectionError "Failed to detect language"
    Just l -> runCommand' (SetFromLanguage (getLanguage l))

runCommand' Exit = throwError $ TErr TraduisonsExit "Exit"

 -- unTraduisons :: ReaderT TraduisonsState (ErrorT TraduisonsError IO) a }
withTokenRefresh :: Traduisons a -> StateT AppState (ErrorT TraduisonsError IO) (Maybe a)
withTokenRefresh f = go (1 :: Int)
  where
    go n = do
      AppState _ _ _ tradState <- get
      result <- liftIO . runTraduisons tradState $ f
      case result of
        Left err@(TErr TokenExpiredError _) ->
          if n > 0
          then renewToken >> go (n-1)
          {- else throwError "No valid access token available" -}
          else throwError err
        Left err -> throwError err
        Right msg -> return $ Just msg

renderError :: TraduisonsError -> String
renderError (TErr flag msg) = case flag of
  ArgumentOutOfRangeException -> "Bad language code: " ++ msg
  UnrecognizedJSONError -> msg
  CurlError -> msg
  NoStringError -> msg
  TraduisonsExit -> msg
  UnknownError -> msg
  LanguageDetectionError -> "Unable to detect language: " ++ msg
  TokenExpiredError -> "Renewing expired token..."
