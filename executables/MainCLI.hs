{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe
import System.Console.Haskeline (InputT, runInputT, defaultSettings, getInputLine)

import Traduisons.Client
import Traduisons.API
import Traduisons.Types

main :: IO ()
main = do
  eitherAppState <- runExceptT createAppState
  case eitherAppState of
    Left err -> putStrLn ("Failed to initialize: " ++ renderError err)
    Right appState -> do
      -- Immediately get an auth token. This lowers our initial response time.
      void $ runExceptT (runStateT renewToken appState)
      runInputT defaultSettings (loop appState)
  where
    loop appState = do
      let promptString = renderAppState appState
      input <- readEvalPrint promptString parseInput
      case input of
        Nothing -> liftIO exitApp
        Just commands -> do
          maybeNewState <- liftIO $ interpretCommands (Just appState) commands
          loop $ fromMaybe appState maybeNewState

exitApp :: IO ()
exitApp = return ()

interpretCommands :: Maybe AppState -> [Command] -> IO (Maybe AppState)
interpretCommands appState commands = do
  let render = msgBody
  result <- runExceptT $ runCommands appState commands
  case result of
    Left err -> putStrLn (renderError err) >> return Nothing
    Right (msg, s) -> do
      when (isJust msg) $ putStrLn . render . fromJust $ msg
      return $ Just s

readEvalPrint :: String -> (String -> a) -> InputT IO (Maybe a)
readEvalPrint prompt f = do
  maybeLine <- getInputLine prompt
  case maybeLine of
    Nothing   -> return Nothing -- EOF / control-d
    Just "?"  -> liftIO $ putStrLn helpMsg >> return (Just (f ""))
    Just line -> return . Just $ f line

renderAppState :: AppState -> String
renderAppState (AppState (Language fL) (Language tL) _ _) =
  fL ++ "|" ++ tL ++ ": "
