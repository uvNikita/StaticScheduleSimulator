{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
) where

import Graphics.QML
import Paths_StaticScheduleSimulator (getDataFileName)

import qualified Data.ByteString.Lazy.Char8 as BS
import           Control.Concurrent.MVar (MVar, putMVar, takeMVar, tryTakeMVar, readMVar, swapMVar, newMVar)
import           Control.Concurrent (forkIO)
import           Control.Monad (void)
import           Data.Typeable (Typeable)
import           Data.Proxy (Proxy(..))
import           Data.Aeson (eitherDecode)
import           Data.Text (Text)
import           Data.Graph.Inductive (isConnected)
import           Data.Graph.Analysis (cyclesIn')
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Graph

type ValidationResult = Maybe Text

data ContextObj = ContextObj { taskValidationResult   :: MVar ValidationResult
                             , systemValidationResult :: MVar ValidationResult
                             , loadedGraph :: MVar Text } deriving Typeable

data TaskValidationDone deriving Typeable
instance SignalKeyClass TaskValidationDone where
    type SignalParams TaskValidationDone = IO ()

data SystemValidationDone deriving Typeable
instance SignalKeyClass SystemValidationDone where
    type SignalParams SystemValidationDone = IO ()

data GraphLoaded deriving Typeable
instance SignalKeyClass GraphLoaded where
    type SignalParams GraphLoaded = IO ()

instance DefaultClass ContextObj where
    classMembers = [
          defPropertySigRO "taskValidationResult"
                           (Proxy :: Proxy TaskValidationDone)
                           (getValidationResult taskValidationResult)

        , defPropertySigRO' "systemValidationResult"
                            (Proxy :: Proxy SystemValidationDone)
                            (getValidationResult systemValidationResult)

        , defMethod "modelate" modelate_

        , defPropertySigRO' "loadedGraph" (Proxy :: Proxy GraphLoaded) $
              readMVar . loadedGraph . fromObjRef

        , defMethod "saveGraphToFile" saveGraphToFile

        , defMethod "loadGraphFromFile" loadGraphFromFile
        ]

type Task   = Directed   Int Int
type System = Undirected Int Int

modelate_ :: ObjRef ContextObj -> Text -> Text -> IO ()
modelate_ ctx task system = void . forkIO $ do
    let taskVar   = taskValidationResult   . fromObjRef $ ctx
    let systemVar = systemValidationResult . fromObjRef $ ctx
    _ <- tryTakeMVar taskVar
    _ <- tryTakeMVar systemVar

    _ <- forkIO $ validateTask_ ctx task
    _ <- forkIO $ validateSystem_ ctx system

    tvr <- readMVar taskVar
    svr <- readMVar systemVar

    case (tvr, svr) of
        (Nothing, Nothing) -> print "OK!!"
        _ -> print "NOT OK!!"

validateTask_ :: ObjRef ContextObj -> Text -> IO ()
validateTask_ ctx graphStr = do
    let resultVar = taskValidationResult . fromObjRef $ ctx
    putMVar resultVar $ validateTask (T.unpack graphStr)
    fireSignal (Proxy :: Proxy TaskValidationDone) ctx

validateSystem_ :: ObjRef ContextObj -> Text -> IO ()
validateSystem_ ctx graphStr = do
    let resultVar = systemValidationResult . fromObjRef $ ctx
    putMVar resultVar $ validateSystem (T.unpack graphStr)
    fireSignal (Proxy :: Proxy SystemValidationDone) ctx

validateTask :: String -> Maybe Text
validateTask graphStr =
    case eitherDecode (BS.pack graphStr) :: Either String Task of
        Left err    -> Just $ T.pack err
        Right graph -> if isDAG graph
                           then Nothing
                           else Just "not acyclic"
    where isDAG = null . cyclesIn'

validateSystem :: String -> Maybe Text
validateSystem graphStr =
    case eitherDecode (BS.pack graphStr) :: Either String System of
        Left err    -> Just $ T.pack err
        Right graph -> if isConnected graph
                           then Nothing
                           else Just "not fully connected"

getValidationResult :: (ContextObj -> MVar ValidationResult) -> ObjRef ContextObj -> IO Text
getValidationResult var ctx = do
    let resultVar = var . fromObjRef $ ctx
    res <- readMVar resultVar
    let msg = maybe "ok" (T.append "error:") res
    return msg

saveGraphToFile :: ObjRef ContextObj -> Text -> Text -> IO ()
saveGraphToFile _ path graph = do
    _ <- forkIO $ TIO.writeFile (T.unpack path) graph
    return ()

loadGraphFromFile :: ObjRef ContextObj -> Text -> IO ()
loadGraphFromFile ctx path = do
    _ <- forkIO $ do
        let resultVar = loadedGraph . fromObjRef $ ctx
        graph <- TIO.readFile (T.unpack path)
        _ <- swapMVar resultVar graph
        fireSignal (Proxy :: Proxy GraphLoaded) ctx
    return ()

main :: IO ()
main = do
    tr <- newMVar Nothing
    sr <- newMVar Nothing
    gr <- newMVar ""
    ctx <- newObjectDC $ ContextObj tr sr gr

    qml <- getDataFileName "qml/Window.qml"
    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument qml
    ,   contextObject = Just $ anyObjRef ctx
    }

