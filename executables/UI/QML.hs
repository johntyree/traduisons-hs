
module UI.QML where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Applicative
import Data.IORef
import Graphics.QML
import qualified Data.Text as T

import Paths_traduisons


data AppState = AS { isLoading :: Bool
                   , langPair :: T.Text
                   , result :: T.Text }
    deriving (Show, Eq)

defaultAppState = AS False (T.pack "en | fi:") (T.pack "")

main :: IO ()
main = do
    qmlPath <- getDataFileName "executables/UI/ui.qml"
    state <- newIORef defaultAppState
    langPairKey <- newSignalKey :: IO (SignalKey (IO ()))
    resultKey <- newSignalKey :: IO (SignalKey (IO ()))
    isLoadingKey <- newSignalKey :: IO (SignalKey (IO ()))
    clazz <- newClass [
        langPairProperty langPairKey state
      , resultProperty resultKey state
      , isLoadingProperty resultKey state
      , factorialMethod isLoadingKey resultKey state
      ]
    ctx <- newObject clazz ()
    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument qmlPath
      , contextObject = Just $ anyObjRef ctx }


resultProperty key state = defPropertySigRO' "result" key $ \_ -> do
    result <$> readIORef state

langPairProperty key state = defPropertySigRO' "langPair" key $ \_ -> do
    langPair <$> readIORef state

isLoadingProperty key state = defPropertySigRO' "isLoading" key $ \_ -> do
    isLoading <$> readIORef state

factorialMethod ilKey rKey state = defMethod' "factorial" $ \obj txt -> do
    let n = read $ T.unpack txt :: Integer
        setWorking s = s { result = T.pack "working ..."
                         , isLoading = True }
    modifyIORef state setWorking
    fireSignals [ilKey, rKey] obj
    void . forkIO $ do
        out <- evaluate $ factorial n
        let setResult r s = s { isLoading = False, result = T.pack (show r) }
        modifyIORef state (setResult out)
        fireSignals [ilKey, rKey] obj

fireSignals keys obj = mapM_ (flip fireSignal obj) $ keys

factorial :: (Num a, Enum a) => a -> a
factorial = product . enumFromTo 1
