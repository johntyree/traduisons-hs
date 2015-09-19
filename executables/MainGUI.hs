{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except

import Traduisons.Client
import qualified UI.QML as Qui

main :: IO ()
main = do
  eitherAppState <- runExceptT createAppState
  case eitherAppState of
    Left err -> putStrLn ("Failed to initialize: " ++ renderError err)
    Right appState -> Qui.runGUI appState
