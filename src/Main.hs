{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Graph
import Shelude

data ContextObj = ContextObj { taskGraphVar   :: MVar (Either Error Task)
                             , systemGraphVar :: MVar (Either Error System)
                             } deriving Typeable

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

        , defMethod "modelate" modelate_

        , defMethod "saveGraphToFile" saveGraphToFile

        , defMethod "loadGraphFromFile" loadGraphFromFile

        , defSignal "modelationFinished" (Proxy :: Proxy ModelationFinished)
        ]

modelate_ :: ObjRef ContextObj -> Text -> Text -> IO ()
modelate_ ctx taskStr systemStr = void . forkIO $ do
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

check :: DynGraph gr => [Validator] -> gr a b -> Either Error (gr a b)
check vs graph = case validate graph vs of
                     [] -> Right graph
                     errs -> Left $ T.intercalate ", " errs

parseTask :: String -> Either Error Task
parseTask graphStr = eitherDecode (BS.pack graphStr) >>= check [DAG]

parseSystem :: String -> Either Error System
parseSystem graphStr =  eitherDecode (BS.pack graphStr) >>= check [Connected]

getValidationResult :: DynGraph gr => (ContextObj -> MVar (Either Error (gr a b)))
                                   -> ObjRef ContextObj
                                   -> IO Text
getValidationResult var ctx = do
    let resultVar = var . fromObjRef $ ctx
    isEmpty <- isEmptyMVar resultVar
    if isEmpty
        then return "init"
        else do
            res <- readMVar resultVar
            let msg = either (T.append "error:") (const "ok") res
            return msg

    --TODO: use tryReadMVar after update base>=4.7
    --res <- tryReadMVar resultVar
    --let getMsg = either (T.append "error:") "ok"
    --let msg = maybe "init" getMsg res
    --return msg

saveGraphToFile :: ObjRef ContextObj -> Text -> Text -> IO ()
saveGraphToFile _ = TIO.writeFile . T.unpack

loadGraphFromFile :: ObjRef ContextObj -> Text -> IO Text
loadGraphFromFile _ path = TIO.readFile (T.unpack path)

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

