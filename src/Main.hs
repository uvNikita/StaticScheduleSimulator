{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (
    main
) where

import Graphics.QML
import Paths_StaticScheduleSimulator (getDataFileName)

import           System.Random (newStdGen)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Control.Concurrent.MVar (MVar, putMVar, tryTakeMVar, isEmptyMVar, readMVar, readMVar, newEmptyMVar)
import           Control.Concurrent (forkIO)
import           Control.Monad (void)
import           Data.Typeable (Typeable)
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import           Data.Graph.Inductive (DynGraph)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Aeson (ToJSON, toJSON, object, (.=), encode)

import Graph
import Shedule

data ContextObj = ContextObj { taskGraphVar   :: MVar (Either [Error] Task)
                             , systemGraphVar :: MVar (Either [Error] System)
                             } deriving Typeable

data ValidationResult = ValidationResult { validationStatus :: Text
                                         , validationErrors :: [Text] }

instance ToJSON ValidationResult where
    toJSON (ValidationResult {..}) = object ["status" .= validationStatus
                                           , "errors" .= validationErrors]

data TaskValidationDone deriving Typeable
instance SignalKeyClass TaskValidationDone where
    type SignalParams TaskValidationDone = IO ()

data SystemValidationDone deriving Typeable
instance SignalKeyClass SystemValidationDone where
    type SignalParams SystemValidationDone = IO ()

data ModelationFinished deriving Typeable
instance SignalKeyClass ModelationFinished where
    type SignalParams ModelationFinished = IO ()

instance DefaultClass ContextObj where
    classMembers = [
          defPropertySigRO "taskValidationResult"
                           (Proxy :: Proxy TaskValidationDone)
                           (getValidationResult taskGraphVar)

        , defPropertySigRO' "systemValidationResult"
                            (Proxy :: Proxy SystemValidationDone)
                            (getValidationResult systemGraphVar)

        , defMethod "modelate" modelate

        , defMethod "saveGraphToFile" saveGraphToFile

        , defMethod "loadGraphFromFile" loadGraphFromFile

        , defMethod "generateTask" generateTask

        , defSignal "modelationFinished" (Proxy :: Proxy ModelationFinished)
        ]

modelate :: ObjRef ContextObj -> Text -> Text -> IO ()
modelate ctx taskStr systemStr = void . forkIO $ do
    let taskVar   = taskGraphVar   . fromObjRef $ ctx
    let systemVar = systemGraphVar . fromObjRef $ ctx
    _ <- tryTakeMVar taskVar
    _ <- tryTakeMVar systemVar

    _ <- forkIO $ parseTask_   ctx taskStr
    _ <- forkIO $ parseSystem_ ctx systemStr

    tvr <- readMVar taskVar
    svr <- readMVar systemVar

    case (tvr, svr) of
        (Right task, Right system) -> do
            rg <- newStdGen
            let queues = genQueues task rg
            print queues
        _ -> return ()
    fireSignal (Proxy :: Proxy ModelationFinished) ctx

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
    isEmpty <- isEmptyMVar resultVar
    res <- if isEmpty
               then return (ValidationResult "init" [])
               else do
                   res <- readMVar resultVar
                   return $ either (ValidationResult "error")
                                   (const $ ValidationResult "ok" [])
                                   res
    return . T.pack . BS.unpack . encode $ res

    --TODO: use tryReadMVar after update base>=4.7
    --res <- tryReadMVar resultVar
    --let getMsg = either (T.append "error:") "ok"
    --let msg = maybe "init" getMsg res
    --return msg

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
    tr <- newEmptyMVar
    sr <- newEmptyMVar
    ctx <- newObjectDC $ ContextObj tr sr

    qml <- getDataFileName "qml/Window.qml"
    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument qml
    ,   contextObject = Just $ anyObjRef ctx
    }

