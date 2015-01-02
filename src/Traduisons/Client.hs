
module Traduisons.Client where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Data.List

import Traduisons.API
import Traduisons.Types

helpMsg :: String
helpMsg = "Help yourself."

createAppState :: IO AppState
createAppState =
  let new = return . AppState (Language "nl") (Language "en") []
  in runErrorT newState >>= either error new

renewToken :: StateT AppState (ErrorT String IO) (Maybe a)
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
            -> IO (Either String (Maybe Message, AppState))
runCommands appState cmds = do
  let app = foldM (const runCommand) Nothing cmds
  initial <- maybe (liftIO createAppState) return appState
  runErrorT . runStateT app $ initial

-- Consider representing errors as an (Error String) value as part of a
-- TranslationResult ADT.
runCommand :: Command -> StateT AppState (ErrorT String IO) (Maybe Message)
runCommand c = do
  msg <- runCommand' c
  modify (\a -> a { asHistory = (c, msg) : asHistory a })
  return msg
runCommand' :: Command -> StateT AppState (ErrorT String IO) (Maybe Message)
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
      AppState fromLang toLang _ tradState <- get
      let message = Message fromLang rawMsg
      translation <- liftIO . runTraduisons tradState $ translate toLang message
      case translation of
        Left err ->
          if n > 0
          then renewToken >> go 0
          else throwError err
        Right msg -> return $ Just msg
runCommand' Exit = throwError "exit"
