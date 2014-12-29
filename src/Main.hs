{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Data.List
import Data.Maybe
import System.Console.Readline (addHistory, readline)

import API
import Types


main :: IO ()
main = createAppState >>= loop
  where
    loop appState = do
      let promptString = renderAppState appState
      input <- readEvalPrint promptString parseInput
      case input of
        Nothing -> exitApp
        Just commands -> do
          maybeNewState <- interpretCommands (Just appState) commands
          loop $ fromMaybe appState maybeNewState

exitApp :: IO ()
exitApp = return ()

interpretCommands :: Maybe AppState -> [Command] -> IO (Maybe AppState)
interpretCommands appState commands = do
  let render = msgBody
  result <- runCommands appState commands
  case result of
    Left err -> putStrLn err >> return Nothing
    Right (msg, s) -> do
      when (isJust msg) $ putStrLn . render . fromJust $ msg
      return $ Just s

readEvalPrint :: String -> (String -> a) -> IO (Maybe a)
readEvalPrint prompt f = do
   maybeLine <- readline prompt
   case maybeLine of
    Nothing   -> return Nothing -- EOF / control-d
    Just "?"  -> putStrLn helpMsg >> return (Just (f ""))
    Just line -> do addHistory line
                    return . Just $ f line

helpMsg :: String
helpMsg = "Help yourself."

runCommands :: Maybe AppState -> [Command]
            -> IO (Either String (Maybe Message, AppState))
runCommands appState cmds = do
  let app = foldM (const runCommand) Nothing cmds
  initial <- maybe (liftIO createAppState) return appState
  runErrorT . runStateT app $ initial

renderAppState :: AppState -> String
renderAppState (AppState (Language fL) (Language tL) _ _) =
  fL ++ "|" ++ tL ++ ": "

parseInput :: String -> [Command]
parseInput ('/':s) = SwapLanguages : parseInput s
parseInput "\EOT"  = [Exit]
parseInput ""      = []
parseInput ('|':s) = [SetToLanguage s]
parseInput s
  | "/" `isSuffixOf` s = SwapLanguages : parseInput (init s)
  | "|" `isInfixOf` s = let (from, '|':to) = break (== '|') s
                            f c l = [c l | not (null l)]
                        in f SetFromLanguage from ++ f SetToLanguage to
  | otherwise = [Translate s]

createAppState :: IO AppState
createAppState =
  let new = return . AppState (Language "nl") (Language "en") []
  in runErrorT newState >>= either error new

-- Consider representing errors as an (Error String) value as part of a
-- TranslationResult ADT.
runCommand :: Command -> StateT AppState (ErrorT String IO) (Maybe Message)
runCommand c =
  modify (\a -> a { asHistory = asHistory a ++ [c] }) >> runCommand_ c

runCommand_ :: Command -> StateT AppState (ErrorT String IO) (Maybe Message)
runCommand_ SwapLanguages = get >>= \aS -> from aS >> to aS
  where from = runCommand_ . SetFromLanguage . getLanguage . asToLang
        to = runCommand_ . SetToLanguage . getLanguage . asFromLang
runCommand_ (SetFromLanguage l) = const Nothing <$> modify setFromLang
  where setFromLang appState = appState { asFromLang = Language l }
runCommand_ (SetToLanguage l) = const Nothing <$> modify setToLang
  where setToLang appState = appState { asToLang = Language l }
runCommand_ (Translate rawMsg) = go (1 :: Int)
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
runCommand_ Exit = throwError "exit"

renewToken :: StateT AppState (ErrorT String IO) (Maybe a)
renewToken = do
  appState <- get
  traduisonsState <- lift newState
  put appState { asTraduisonsState = traduisonsState }
  return Nothing
