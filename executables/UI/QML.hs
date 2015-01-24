{-# LANGUAGE OverloadedStrings #-}

module UI.QML where

import Prelude hiding (div)
import Control.Concurrent
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Control.Applicative
import Data.Either
import Data.IORef
import Data.List
import Data.List.Split
import Data.Maybe
import Graphics.QML
import qualified Data.Text as T

import Paths_traduisons

import Traduisons.API
import Traduisons.Client
import Traduisons.Types

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
    asyncRenewToken initialAppState
    guiAppState <- newIORef (defaultGUIAppState [initialAppState])
    langPairSignal <- newSignalKey
    gasResultSignal <- newSignalKey
    gasIsLoadingSignal <- newSignalKey
    isErrorSignal <- newSignalKey
    let signals = [langPairSignal, isErrorSignal,
                   gasResultSignal, gasIsLoadingSignal]
        signals :: [SignalKey (IO ())]
    cls <- newClass [
        defPropertySigRO' "result" gasResultSignal $ \_ ->
          gasResultProperty guiAppState
      , defPropertySigRO' "clipboardContents" gasResultSignal $ \_ ->
          clipboardContentsProperty guiAppState
      , defPropertySigRO' "langPair" langPairSignal $ \_ ->
          langPairProperty guiAppState
      , defPropertySigRO' "isLoading" gasIsLoadingSignal $ \_ ->
          gasIsLoadingProperty guiAppState
      , defPropertySigRO' "isError" isErrorSignal $ \_ ->
          isErrorProperty guiAppState
      , defMethod' "handleInput" $ \obj txt -> do
          let update = fireSignals signals obj
          handleInputMethod update guiAppState txt
      ]
    ctx <- newObject cls ()
    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument qmlPath
      , contextObject = Just $ anyObjRef ctx }
    shutdownQt
  where
    fireSignals keys obj = mapM_ (`fireSignal` obj) keys

asyncRenewToken :: AppState -> IO ()
asyncRenewToken = void . forkIO . void . runErrorT  . runStateT renewToken

clipboardContentsProperty :: IORef GUIAppState -> IO T.Text
clipboardContentsProperty guiAppState =  render <$> readIORef guiAppState
  where render = either (const "") id . gasResult

gasResultProperty :: IORef GUIAppState -> IO T.Text
gasResultProperty guiAppState = do
  gas <- readIORef guiAppState
  return $ case gasResult gas of
    Left msg -> asRichText (renderError msg)
    Right _ -> renderHistory (gasAppStates gas)

asRichText :: String -> T.Text
asRichText s = T.pack $ concat renderedLines
  where
    lines' = splitOn "\n" s
    renderedLines = div hangingIndent [intercalate "<br/>" lines']


newtype Style = Style { unStyle :: String }

hangingIndent :: Style
hangingIndent = Style "margin-left: 8px; text-indent: -8px"

indent :: Style
indent = Style "margin-left: 5px"

div :: Style -> [String] -> [String]
div (Style s) l = ["<div style=\"" ++ s ++ "\">"] ++ l ++ ["</div>"]

renderHistory :: [AppState] -> T.Text
renderHistory appStates = T.unlines renderedLines
    where
        renderedLines =  concatMap (fromMaybe [] . render) (reverse appStates)
        render :: AppState -> Maybe [T.Text]
        render appState = do
            fM <- fmap fromMsg <$> listToMaybe . asHistory $ appState
            tM <- fmap toMsg <$> listToMaybe . asHistory $ appState
            guard $ not (null tM)
            guard $ not (null fM)
            let fL = getLanguage . asFromLang $ appState
                tL = getLanguage . asToLang $ appState
                -- FIXME: This is going to require HTML escaping...
                from = div hangingIndent [color "darkred" fL, ": ", fM]
                to = div hangingIndent [color "darkblue" tL, ": ", tM]
            return $ map build [from, div indent to]
        build = T.pack . concat
        color c t = concat ["<span style=\"color: ", c, "\">" , t , "</span>"]
        toMsg (_, Just msg) = msgBody msg
        toMsg _ = ""
        fromMsg (Translate s, _) = s
        fromMsg _ = ""

langPairProperty :: IORef GUIAppState -> IO T.Text
langPairProperty guiAppState = render . listToMaybe . gasAppStates <$> readIORef guiAppState
    where render :: Maybe AppState -> T.Text
          render Nothing = "???"
          render (Just appState) =
            let fr = getLanguage . asFromLang $ appState
                to = getLanguage . asToLang $ appState
            in T.concat . map T.pack $ [fr, " | ", to, ":"]

gasIsLoadingProperty :: IORef GUIAppState -> IO Bool
gasIsLoadingProperty guiAppState = gasIsLoading <$> readIORef guiAppState

isErrorProperty :: IORef GUIAppState -> IO Bool
isErrorProperty guiAppState = isLeft . gasResult <$> readIORef guiAppState

handleInputMethod :: IO () -> IORef GUIAppState -> T.Text -> IO ()
handleInputMethod updateGUI guiAppState txt = do
    modifyIORef guiAppState $ \s -> s { gasIsLoading = True }
    void updateGUI
    let execute = readIORef guiAppState >>= handleInput txt >>= writeIORef guiAppState
    void . forkIO $ execute >> updateGUI

handleInput :: T.Text -> GUIAppState -> IO GUIAppState
handleInput input initialGuiAppState = do
    let commands = parseInput (T.unpack input)
        appStates = gasAppStates guiAppState
        initialAppState = listToMaybe appStates
        guiAppState = initialGuiAppState { gasIsLoading = False }
    result <- runErrorT $ runCommands initialAppState commands
    case result of
        Left err -> return $ guiAppState { gasResult = Left err }
        Right (msg, appState) -> return $
          -- If no new and old is err, then old else new
          let oldResult = gasResult guiAppState
              maybeNewResult = T.pack . msgBody <$> msg
              newResult = Right $ fromMaybe "" maybeNewResult
              finalResult = if isLeft oldResult && isNothing maybeNewResult
                            then oldResult
                            else newResult
          in guiAppState { gasResult = finalResult
                         , gasAppStates = appState : appStates }
