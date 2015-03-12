{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
) where

import Graphics.QML
import Paths_StaticScheduleSimulator (getDataFileName)

import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Concurrent.MVar (MVar, swapMVar, readMVar, newMVar)
import Control.Concurrent (forkIO)
import Data.Typeable (Typeable)
import Data.Proxy (Proxy(..))
import Data.Aeson (eitherDecode)
import Data.Text (Text)
import Data.Graph.Inductive (isConnected)
import Data.Graph.Analysis (cyclesIn')
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Graph

data ContextObj = ContextObj { taskGraphResult :: MVar Text
                             , systemGraphResult :: MVar Text
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
          defPropertySigRO "taskGraphResult" (Proxy :: Proxy TaskValidationDone) $
              readMVar . taskGraphResult . fromObjRef
        , defPropertySigRO' "systemGraphResult" (Proxy :: Proxy SystemValidationDone) $
              readMVar . systemGraphResult . fromObjRef
        , defPropertySigRO' "loadedGraph" (Proxy :: Proxy GraphLoaded) $
              readMVar . loadedGraph . fromObjRef
        , defMethod "validateTask"   validateTask_
        , defMethod "validateSystem" validateSystem_
        , defMethod "saveGraphToFile" saveGraphToFile
        , defMethod "loadGraphFromFile" loadGraphFromFile
        ]

type Task   = Directed   Int Int
type System = Undirected Int Int

validateTask_ :: ObjRef ContextObj -> Text -> IO ()
validateTask_ ctx graphStr = do
    _ <- forkIO $ do
        let resultVar = taskGraphResult . fromObjRef $ ctx
        _ <- case validateTask (T.unpack graphStr) of
            Just err -> swapMVar resultVar (T.concat ["error: ", T.pack err])
            Nothing  -> swapMVar resultVar "ok"
        fireSignal (Proxy :: Proxy TaskValidationDone) ctx
    return ()

validateSystem_ :: ObjRef ContextObj -> Text -> IO ()
validateSystem_ ctx graphStr = do
    _ <- forkIO $ do
        let resultVar = systemGraphResult . fromObjRef $ ctx
        _ <- case validateSystem (T.unpack graphStr) of
            Just err -> swapMVar resultVar (T.concat ["error: ", T.pack err])
            Nothing  -> swapMVar resultVar "ok"
        fireSignal (Proxy :: Proxy SystemValidationDone) ctx
    return ()

validateTask :: String -> Maybe String
validateTask graphStr =
    case eitherDecode (BS.pack graphStr) :: Either String Task of
        Left err    -> Just err
        Right graph -> if isDAG graph
                           then Nothing
                           else Just "not acyclic"
    where isDAG = null . cyclesIn'

validateSystem :: String -> Maybe String
validateSystem graphStr =
    case eitherDecode (BS.pack graphStr) :: Either String System of
        Left err    -> Just err
        Right graph -> if isConnected graph
                           then Nothing
                           else Just "not fully connected"

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
    tr <- newMVar "Init"
    sr <- newMVar "Init"
    gr <- newMVar ""
    ctx <- newObjectDC $ ContextObj tr sr gr

    qml <- getDataFileName "qml/Window.qml"
    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument qml
    ,   contextObject = Just $ anyObjRef ctx
    }

