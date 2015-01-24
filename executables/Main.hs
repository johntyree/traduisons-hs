{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Data.Maybe
import System.Console.Readline (addHistory, readline)

import Traduisons.Client
import Traduisons.API
import Traduisons.Types
import qualified UI.QML as Qui

main :: IO ()
main = mainGUI

mainGUI :: IO ()
mainGUI = do
  eitherAppState <- runErrorT createAppState
  case eitherAppState of
    Left err -> putStrLn ("Failed to initialize: " ++ renderError err)
    Right appState -> Qui.runGUI appState

mainCLI :: IO ()
mainCLI = do
  eitherAppState <- runErrorT createAppState
  case eitherAppState of
    Left err -> putStrLn ("Failed to initialize: " ++ renderError err)
    Right appState -> do
      void $ runErrorT (runStateT renewToken appState)
      loop appState
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
  result <- runErrorT $ runCommands appState commands
  case result of
    Left err -> putStrLn (renderError err) >> return Nothing
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

renderAppState :: AppState -> String
renderAppState (AppState (Language fL) (Language tL) _ _) =
  fL ++ "|" ++ tL ++ ": "
