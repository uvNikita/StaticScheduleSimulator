{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

module Main (
    main
) where

import           Graphics.QML
import           Paths_StaticScheduleSimulator (getDataFileName)

import           Control.Applicative           ((<$>))
import           Control.Concurrent            (forkIO)
import           Control.Concurrent.MVar       (MVar, isEmptyMVar, newEmptyMVar,
                                                putMVar, readMVar, readMVar,
                                                tryTakeMVar)
import           Control.Monad                 (void)
import           Data.Aeson                    (FromJSON, ToJSON, Value (Null),
                                                decode, encode, object, toJSON,
                                                (.=))
import qualified Data.ByteString.Lazy.Char8    as BS
import           Data.Graph.Inductive          (DynGraph)
import           Data.Maybe                    (fromJust)
import           Data.Proxy                    (Proxy (..))
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Typeable                 (Typeable)
import           System.Random                 (newStdGen)

import           Graph
import           Shedule

data ContextObj = ContextObj { taskGraphVar   :: MVar (Either [Error] Task)
                             , systemGraphVar :: MVar (Either [Error] System)
                             , simulationVar  :: MVar Simulation } deriving Typeable

data ValidationResult = ValidationResult { validationStatus :: Text
                                         , validationErrors :: [Text]
                                         }

instance ToJSON ValidationResult where
    toJSON (ValidationResult {..}) = object ["status" .= validationStatus
                                           , "errors" .= validationErrors]

data TaskValidationDone deriving Typeable
instance SignalKeyClass TaskValidationDone where
    type SignalParams TaskValidationDone = IO ()

data SystemValidationDone deriving Typeable
instance SignalKeyClass SystemValidationDone where
    type SignalParams SystemValidationDone = IO ()

data SimulationFinished deriving Typeable
instance SignalKeyClass SimulationFinished where
    type SignalParams SimulationFinished = IO ()

instance DefaultClass ContextObj where
    classMembers = [
          defPropertySigRO "taskValidationResult"
                           (Proxy :: Proxy TaskValidationDone)
                           (getValidationResult taskGraphVar)
        , defPropertySigRO "systemValidationResult"
                           (Proxy :: Proxy SystemValidationDone)
                           (getValidationResult systemGraphVar)
        , defPropertySigRO "simulationResult"
                           (Proxy :: Proxy SimulationFinished)
                           getSimulationResult

        , defMethod "simulate"          doSimulate
        , defMethod "saveGraphToFile"   saveGraphToFile
        , defMethod "loadGraphFromFile" loadGraphFromFile
        , defMethod "generateTask"      generateTask

        , defSignal "simulationFinished" (Proxy :: Proxy SimulationFinished)
        ]

doSimulate :: ObjRef ContextObj -> Text -> Text -> Text -> IO ()
doSimulate ctx taskStr systemStr configStr = void . forkIO $ do
    let taskVar   = taskGraphVar   . fromObjRef $ ctx
    let systemVar = systemGraphVar . fromObjRef $ ctx
    let simVar    = simulationVar  . fromObjRef $ ctx
    _ <- tryTakeMVar taskVar
    _ <- tryTakeMVar systemVar
    _ <- tryTakeMVar simVar

    _ <- forkIO $ parseTask_   ctx taskStr
    _ <- forkIO $ parseSystem_ ctx systemStr

    tvr <- readMVar taskVar
    svr <- readMVar systemVar

    case (tvr, svr) of
        (Right task, Right system) -> do
            let config     = fromJust . decode . BS.pack . T.unpack $ configStr
            print config
            let simulation = simulate config system task
            putMVar simVar simulation
        _ -> return ()
    fireSignal (Proxy :: Proxy SimulationFinished) ctx


getSimulationResult :: ObjRef ContextObj -> IO Text
getSimulationResult ctx = do
    maybeRes <- tryReadMVar . simulationVar . fromObjRef $ ctx
    let resJSON = case maybeRes of
                      Nothing  -> Null
                      Just res -> toJSON res
    return . T.pack . BS.unpack . encode $ resJSON

parseTask_ :: ObjRef ContextObj -> Text -> IO ()
parseTask_ ctx graphStr = do
    let resultVar = taskGraphVar . fromObjRef $ ctx
    putMVar resultVar $ parseTask (T.unpack graphStr)
    fireSignal (Proxy :: Proxy TaskValidationDone) ctx

parseSystem_ :: ObjRef ContextObj -> Text -> IO ()
parseSystem_ ctx graphStr = do
    let resultVar = systemGraphVar . fromObjRef $ ctx
    putMVar resultVar $ parseSystem (T.unpack graphStr)
    fireSignal (Proxy :: Proxy SystemValidationDone) ctx

parseGraph :: (DynGraph gr, FromJSON (gr a b)) => [Validator] -> String -> Either [Text] (gr a b)
parseGraph validators graphStr =
    case eitherDecode (BS.pack graphStr) of
        Left e   -> Left [e]
        Right gr -> case validate gr validators of
                        [] -> Right gr
                        es -> Left es

parseTask :: String -> Either [Error] Task
parseTask = parseGraph [NonEmpty, DAG]

parseSystem :: String -> Either [Error] System
parseSystem = parseGraph [NonEmpty, Connected]

getValidationResult :: DynGraph gr => (ContextObj -> MVar (Either [Error] (gr a b)))
                                   -> ObjRef ContextObj
                                   -> IO Text
getValidationResult var ctx = do
    let resultVar = var . fromObjRef $ ctx
    maybeRes <- tryReadMVar resultVar
    let vr = case maybeRes of
                 Nothing  -> ValidationResult "init" []
                 Just res -> either (ValidationResult "error") (const $ ValidationResult "ok" []) res
    return . T.pack . BS.unpack . encode $ vr

tryReadMVar :: MVar a -> IO (Maybe a) -- TODO: use library tryReadMVar after update base>=4.7
tryReadMVar var = do
    isEmpty <- isEmptyMVar var
    if isEmpty
        then return Nothing
        else Just <$> readMVar var

saveGraphToFile :: ObjRef ContextObj -> Text -> Text -> IO ()
saveGraphToFile _ = TIO.writeFile . T.unpack

loadGraphFromFile :: ObjRef ContextObj -> Text -> IO Text
loadGraphFromFile _ path = TIO.readFile (T.unpack path)

generateTask :: ObjRef ContextObj -> Int -> Int -> Int -> Int -> Int -> Double -> IO Text
generateTask _ nodeMin nodeMax edgeMin edgeMax nodesCount correlation = do
    rGen <- newStdGen
    let graph = generate (nodeMin, nodeMax) (edgeMin, edgeMax) nodesCount correlation rGen :: Task
    return . T.pack . BS.unpack . encode . toJSON $ graph

main :: IO ()
main = do
    taskGraphVar   <- newEmptyMVar
    systemGraphVar <- newEmptyMVar
    simulationVar  <- newEmptyMVar
    ctx <- newObjectDC ContextObj {..}

    qml <- getDataFileName "qml/Window.qml"
    runEngineLoop defaultEngineConfig { initialDocument = fileDocument qml
                                      , contextObject   = Just $ anyObjRef ctx }

