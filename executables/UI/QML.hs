{-# LANGUAGE OverloadedStrings #-}

module UI.QML where

import Prelude hiding (div)
import Control.Concurrent
import Control.Monad
import Control.Applicative
import Data.Either
import Data.IORef
import Data.List
import Data.List.Split
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
    langPairSignal <- newSignalKey
    gasResultSignal <- newSignalKey
    gasIsLoadingSignal <- newSignalKey
    isErrorSignal <- newSignalKey
    let signals = [langPairSignal, isErrorSignal,
                   gasResultSignal, gasIsLoadingSignal]
        signals :: [SignalKey (IO ())]
    cls <- newClass [
        defPropertySigRO' "result" gasResultSignal $ \_ ->
          gasResultProperty state
      , defPropertySigRO' "clipboardContents" gasResultSignal $ \_ ->
          clipboardContentsProperty state
      , defPropertySigRO' "langPair" langPairSignal $ \_ ->
          langPairProperty state
      , defPropertySigRO' "isLoading" gasIsLoadingSignal $ \_ ->
          gasIsLoadingProperty state
      , defPropertySigRO' "isError" isErrorSignal $ \_ ->
          isErrorProperty state
      , defMethod' "handleInput" $ \obj txt -> do
          let update = fireSignals signals obj
          handleInputMethod update state txt
      ]
    ctx <- newObject cls ()
    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument qmlPath
      , contextObject = Just $ anyObjRef ctx }
    shutdownQt
  where
    fireSignals keys obj = mapM_ (`fireSignal` obj) keys

clipboardContentsProperty :: IORef GUIAppState -> IO T.Text
clipboardContentsProperty state =  render <$> readIORef state
  where render = either (const "") id . gasResult

gasResultProperty :: IORef GUIAppState -> IO T.Text
gasResultProperty state = do
  gas <- readIORef state
  return $ case gasResult gas of
    Left msg -> asRichText (renderError msg)
    Right _ -> renderHistory (gasAppStates gas)

asRichText :: String -> T.Text
asRichText s = T.pack $ concat renderedLines
  where
    lines' = splitOn "\n" s
    renderedLines = div hangingIndent [intercalate "<br/>" lines']

hangingIndent :: String
hangingIndent = "margin-left: 8px; text-indent: -8px"

indent :: String
indent = "margin-left: 5px"

div :: String -> [String] -> [String]
div s l = ["<div style=\"" ++ s ++ "\">"] ++ l ++ ["</div>"]

renderHistory :: [AppState] -> T.Text
renderHistory appStates = T.unlines renderedLines
    where
        renderedLines =  concatMap (fromMaybe [] . render) (reverse appStates)
        render :: AppState -> Maybe [T.Text]
        render appState = do
            fM <- fmap fromMsg <$> safeHead . asHistory $ appState
            tM <- fmap toMsg <$> safeHead . asHistory $ appState
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
langPairProperty state = render . safeHead . gasAppStates <$> readIORef state
    where render :: Maybe AppState -> T.Text
          render Nothing = "???"
          render (Just appState) =
            let fr = getLanguage . asFromLang $ appState
                to = getLanguage . asToLang $ appState
            in T.concat . map T.pack $ [fr, " | ", to, ":"]

gasIsLoadingProperty :: IORef GUIAppState -> IO Bool
gasIsLoadingProperty state = gasIsLoading <$> readIORef state

isErrorProperty :: IORef GUIAppState -> IO Bool
isErrorProperty state = isLeft . gasResult <$> readIORef state

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
          -- If no new and old is err, then old else new
          let oldResult = gasResult guiAppState
              maybeNewResult = T.pack . msgBody <$> msg
              newResult = Right $ fromMaybe "" maybeNewResult
              finalResult = if isLeft oldResult && isNothing maybeNewResult
                            then oldResult
                            else newResult
          in guiAppState { gasResult = finalResult
                         , gasAppStates = appState : appStates }
