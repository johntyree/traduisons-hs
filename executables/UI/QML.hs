{-# LANGUAGE OverloadedStrings #-}

module UI.QML where

import Control.Concurrent
import Control.Monad
import Control.Applicative
import Data.Either
import Data.IORef
import Data.Maybe
import Graphics.QML
import qualified Data.Text as T

import Paths_traduisons

import Traduisons.Types
import Traduisons.Client
import Traduisons.Util (safeHead)

type GUIAppStateResult = Either TraduisonsError T.Text

data GUIAppState = GAS { gasIsLoading :: Bool
                       , gasResult :: GUIAppStateResult
                       , gasAppStates :: [AppState] }
    deriving (Show, Eq)

defaultGUIAppState :: [AppState] -> GUIAppState
defaultGUIAppState = GAS False (Right "")

runGUI :: AppState -> IO ()
runGUI initialAppState = do
    qmlPath <- getDataFileName "executables/UI/ui.qml"
    state <- newIORef (defaultGUIAppState [initialAppState])
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
  gas <- readIORef state
  return $ case (gasResult gas) of
    Left msg -> T.pack (renderError msg)
    Right _ -> renderHistory (gasAppStates gas)

renderHistory :: [AppState] -> T.Text
renderHistory appStates = T.unlines $ concatMap (fromMaybe [] . render) appStates
    where
        render :: AppState -> Maybe [T.Text]
        render appState = do
            fM <- fmap fromMsg <$> safeHead . asHistory $ appState
            tM <- fmap toMsg <$> safeHead . asHistory $ appState
            guard $ not (null tM)
            guard $ not (null fM)
            let fL = getLanguage . asFromLang $ appState
                tL = getLanguage . asToLang $ appState
                build = T.concat . map T.pack
            return $ map build [[fL, ": ", fM], ["  ", tL, ": ", tM]]
        toMsg (_, Just msg) = msgBody msg
        toMsg _ = ""
        fromMsg (Translate s, _) = s
        fromMsg _ = ""

langPairProperty :: IORef GUIAppState -> IO T.Text
langPairProperty state = render . safeHead . gasAppStates <$> readIORef state
    where render :: Maybe AppState -> T.Text
          render Nothing = "???"
          render (Just appState) =
            let fr = getLanguage . asFromLang $ appState
                to = getLanguage . asToLang $ appState
            in T.concat . map T.pack $ [fr, " | ", to, ":"]

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
        appStates = gasAppStates guiAppState
        initialAppState = safeHead appStates
        guiAppState = initialGuiAppState { gasIsLoading = False }
    result <- runCommands initialAppState commands
    case result of
        Left err -> return $ guiAppState { gasResult = Left err }
        Right (msg, appState) -> return $
          let oldResult = gasResult guiAppState
              maybeNewResult = Right . T.pack . msgBody <$> msg
              newResult = fromMaybe oldResult maybeNewResult
          in guiAppState { gasResult = newResult
                         , gasAppStates = appState : appStates }
