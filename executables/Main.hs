{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Maybe
import System.Console.Readline (addHistory, readline)

import Traduisons.Client
import Traduisons.Types
import qualified UI.QML as Qui

main :: IO ()
main = mainGUI

mainGUI :: IO ()
mainGUI = Qui.main

mainCLI :: IO ()
mainCLI = createAppState >>= loop
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

renderAppState :: AppState -> String
renderAppState (AppState (Language fL) (Language tL) _ _) =
  fL ++ "|" ++ tL ++ ": "
