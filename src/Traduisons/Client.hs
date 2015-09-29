
module Traduisons.Client where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M
import Data.List
import Data.List.Split

import Traduisons.API
import Traduisons.Types
import Traduisons.Util

helpMsg :: String
helpMsg = "Help yourself."

runTest :: String -> ExceptT TraduisonsError IO (Maybe Message, AppState)
runTest input = do
  let commands = concatMap parseInput $ splitOn ";" input
  runCommands Nothing commands

createAppState :: ExceptT TraduisonsError IO AppState
createAppState = liftIO mkTraduisonsState >>= new
  where new = return . AppState (Language "auto") (Language "en") [] M.empty

updateLanguageMap :: AppState -> ExceptT TraduisonsError IO AppState
updateLanguageMap appState = do
  let tState = asTraduisonsState appState
  langResponse <- lift $ runTraduisons tState getLanguagesForTranslate -- unfuck this and replace with the library I added.
  langs <- M.insert "auto" "auto" <$> liftEither langResponse
  return appState { asLanguageNameCodes = langs }

parseInput :: String -> [Command]
parseInput ('/':s) = SwapLanguages : parseInput s
parseInput "\EOT"  = [Exit]
parseInput "?"  = [Help]
parseInput ""      = []
parseInput ('|':s) = [SetToLanguage s]
parseInput s
  | "/" `isSuffixOf` s = SwapLanguages : parseInput (init s)
  | "|" `isInfixOf` s = let (from, '|':to) = break (== '|') s
                            f ctor l = [ctor l | not (null l)]
                        in f SetFromLanguage from ++ f SetToLanguage to
  | otherwise = [Translate s]

runCommands :: Maybe AppState -> [Command]
            -> ExceptT TraduisonsError IO (Maybe Message, AppState)
runCommands appState cmds = do
  let app = foldM (const runCommand) Nothing cmds
  initial <- maybe createAppState return appState
  runStateT app initial

runCommand :: Command -> StateT AppState (ExceptT TraduisonsError IO) (Maybe Message)
runCommand c = do
  msg <- runCommand' c
  modify (\a -> a { asHistory = (c, msg) : asHistory a })
  return msg

runCommand' :: Command -> StateT AppState (ExceptT TraduisonsError IO) (Maybe Message)
runCommand' Exit = throwError $ TErr TraduisonsExit "Exit"
runCommand' Help = get >>= throwError . TErr TraduisonsHelp . renderLanguageNames
runCommand' SwapLanguages = get >>= \aS -> from aS >> to aS
  where from = runCommand' . SetFromLanguage . getLanguage . asToLang
        to = runCommand' . SetToLanguage . getLanguage . asFromLang

runCommand' (SetFromLanguage l) = const Nothing <$> modify setFromLang
  where setFromLang appState = appState { asFromLang = Language l }

runCommand' (SetToLanguage l) = const Nothing <$> modify setToLang
  where setToLang appState = appState { asToLang = Language l }

runCommand' (Translate rawMsg) = do
  AppState fromLang' _ _ _ _ <- get
  when (getLanguage fromLang' == "auto") $ void $ runCommand' (DetectLanguage rawMsg)
  AppState fromLang toLang _ _ _ <- get
  let message = Message fromLang rawMsg
  withTokenRefresh $ translate toLang message

runCommand' (DetectLanguage rawMsg) = do
  result <- withTokenRefresh (detectLanguage rawMsg)
  case result of
    Nothing -> throwError $ TErr LanguageDetectionError "Failed to detect language"
    Just l -> runCommand' (SetFromLanguage (getLanguage l))

renderError :: TraduisonsError -> String
renderError (TErr flag msg) = case flag of
  ArgumentOutOfRangeException -> "Bad language code: " ++ msg
  LanguageDetectionError -> "Unable to detect language: " ++ msg
  ArgumentException -> "Renewing expired token..."
  TraduisonsHelp -> msg
  e -> show e ++ ": " ++ msg

renderLanguageNames :: AppState -> String
renderLanguageNames (AppState (Language fL) (Language tL) _ m _) =
  case (fName, tName) of
    (Just f, Just t) -> f ++ "|" ++ t
    _ -> ""
  where fName = M.lookup fL m
        tName = M.lookup tL m
