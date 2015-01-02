{-# LANGUAGE OverloadedStrings #-}

module UI.QML where

import Control.Concurrent
import Control.Monad
import Control.Applicative
import Data.IORef
import Data.Maybe
import Graphics.QML
import qualified Data.Text as T

import Paths_traduisons

import Traduisons.Types
import Traduisons.Client

type GUIAppStateResult = Either T.Text T.Text

data GUIAppState = GAS { gasIsLoading :: Bool
                       , gasResult :: GUIAppStateResult
                       , gasAppState :: AppState }
    deriving (Show, Eq)

defaultGUIAppState :: AppState -> GUIAppState
defaultGUIAppState = GAS False (Right "")

runGUI :: AppState -> IO ()
runGUI initialAppState = do
    qmlPath <- getDataFileName "executables/UI/ui.qml"
    state <- newIORef (defaultGUIAppState initialAppState)
    langPairKey <- newSignalKey :: IO (SignalKey (IO ()))
    gasResultKey <- newSignalKey :: IO (SignalKey (IO ()))
    gasIsLoadingKey <- newSignalKey :: IO (SignalKey (IO ()))
    clazz <- newClass [
        defPropertySigRO' "result" gasResultKey $ \_ -> do
          gasResultProperty state
      , defPropertySigRO' "langPair" langPairKey $ \_ -> do
          langPairProperty state
      , defPropertySigRO' "isLoading" gasIsLoadingKey $ \_ -> do
          gasIsLoadingProperty state
      , defMethod' "handleInput" $ \obj txt -> do
          let update = fireSignals keys obj
              keys = [langPairKey, gasResultKey, gasIsLoadingKey]
          handleInputMethod update state txt
      ]
    ctx <- newObject clazz ()
    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument qmlPath
      , contextObject = Just $ anyObjRef ctx }
  where
    fireSignals keys obj = mapM_ (flip fireSignal obj) $ keys

gasResultProperty :: IORef GUIAppState -> IO T.Text
gasResultProperty state = do
    s <- gasResult <$> readIORef state
    either return return s

langPairProperty :: IORef GUIAppState -> IO T.Text
langPairProperty state = do
    appState <- gasAppState <$> readIORef state
    let fr = getLanguage . asFromLang $ appState
        to = getLanguage . asToLang $ appState
    return . T.concat . map T.pack $ [fr, " | ", to, ":"]

gasIsLoadingProperty :: IORef GUIAppState -> IO Bool
gasIsLoadingProperty state = gasIsLoading <$> readIORef state

handleInputMethod :: IO () -> IORef GUIAppState -> T.Text -> IO ()
handleInputMethod updateGUI state txt = do
    modifyIORef state $ \s -> s { gasIsLoading = True }
    void updateGUI
    let execute = readIORef state >>= handleInput txt >>= writeIORef state
    void . forkIO $ execute >> updateGUI

handleInput :: T.Text -> GUIAppState -> IO GUIAppState
handleInput input initialGuiAppState = do
    let commands = parseInput (T.unpack input)
        initialAppState = Just (gasAppState guiAppState)
        guiAppState = initialGuiAppState { gasIsLoading = False }
        oldMsg = either (const "") id $ gasResult guiAppState
        newMsg m = fromMaybe oldMsg (T.pack . msgBody <$> m)
    result <- runCommands initialAppState commands
    case result of
        Left err -> return $ guiAppState { gasResult = Left (T.pack err) }
        Right (msg, appState) -> return
            guiAppState { gasResult = Right (newMsg msg)
                        , gasAppState = appState }
