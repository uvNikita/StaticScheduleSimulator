{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}

module Shedule (
      genQueues
    , Task
    , System
    , simulate
) where

import           Data.Function                   (on)
import           Data.Graph.Analysis             (pathTree)
import           Data.Graph.Inductive            (DynGraph, Graph, Node, esp,
                                                  grev, lab, lpre, match,
                                                  neighbors, nodes, pre)
import           Data.IntervalMap.Generic.Strict (Interval, IntervalMap,
                                                  lowerBound, rightClosed,
                                                  upperBound, within)
import qualified Data.IntervalMap.Generic.Strict as IMap
import           Data.List                       (delete, minimumBy, sortBy)
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (fromJust)
import           Data.Set                        (member)
import qualified Data.Set                        as S


import           System.Random                   (StdGen)
import           System.Random.Shuffle           (shuffle')

import           Control.Applicative             ((<$>))
import           Control.Arrow                   (first)
import           Control.Monad                   (filterM)
import           Control.Monad.Loops             (whileM_)
import           Control.Monad.State             (execState, get, modify)

import           Graph                           (Directed, Undirected)

newtype TaskQueue = TaskQueue { unTaskQueue :: [Node] } deriving (Eq, Show)
newtype NodeQueue = NodeQueue { unNodeQueue :: [Node] } deriving (Eq, Show)

type TaskID = Int
type NodeID = Int
type Task   = Directed   Int Int
type System = Undirected Int Int

data QueueGen = DiffQueue | CritPathQueue | RandomQueue StdGen

newtype Simulation = Simulation { unSimulation :: Map Node NodeFlow } deriving (Show)

data SimulationConfig = SimulationConfig { linksCount     :: Int
                                         , connectionType :: ConnectionType
                                         , queueGen       :: QueueGen
                                         , simulationType :: SimulationType }
data SimulationType = WithPreTransfers | WithoutPreTransfers -- TODO improve name

data ConnectionType = Duplex | HalfDuplex

data NodeFlow = NodeFlow { cpuFlow :: CPUFlow, linkFlows :: [LinkFlow] } deriving (Show)

type CPUFlow = Flow CPUFlowInfo
type LinkFlow = Flow LinkFlowInfo

type Flow a = IntervalMap (Ticks, Ticks) a

type Ticks = Int

data CPUFlowInfo  = CPUFlowInfo  { cpuTaskId :: Int } deriving (Show)

data LinkFlowInfo = LinkFlowInfo { linkFromId :: Int
                                 , linkToId   :: Int
                                 , linkTaskId :: Int } deriving (Show)

data SimulationState = SimulationState { currTime       :: Ticks
                                       , currTaskQueue  :: TaskQueue
                                       , currSimulation :: Map Node NodeFlow
                                       , taskNode       :: Map TaskID NodeID }

data Transfer = Transfer Int [NodeID]

instance Ord e => Interval (e,e) e where
    lowerBound (a,_) = a
    upperBound (_,b) = b
    rightClosed _ = False

simulate :: SimulationConfig -> System -> Task -> Simulation
simulate (SimulationConfig {..}) systemGraph taskGraph = Simulation . currSimulation $ execState step initState
    where tq = taskQueue taskGraph queueGen
          nq = nodeQueue systemGraph
          initState = SimulationState { currTime       = 0
                                      , currTaskQueue  = tq
                                      , currSimulation = initSimulation
                                      , taskNode       = Map.empty }
          initSimulation = Map.fromList $ map (\ n -> (n, emptyNodeFlow)) (nodes systemGraph)
          emptyNodeFlow = NodeFlow emptyFlow (replicate linksCount emptyFlow)
          emptyFlow = IMap.empty
          isFinished = null . unTaskQueue . currTaskQueue <$> get
          updateTime newTime = modify (\ st -> st { currTime = newTime })

          getCpuFlows = do
              SimulationState { currSimulation } <- get
              return $ map cpuFlow (Map.elems currSimulation)

          getNewTime = do
              SimulationState { currTime } <- get
              cpuFlows <- getCpuFlows
              let futureTasks = map (`within` (currTime, maxBound)) cpuFlows
              let finishTimes = map (snd . fst . head) . filter (not . null) $ futureTasks
              return $ minimum finishTimes

          getReadyTasks = do
              --SimulationState { currTime } <- get
              finishedTasks <- getFinishedTasks
              let isReady task = all (`member` finishedTasks) (pre taskGraph task)
              SimulationState { currTaskQueue } <- get
              return $ filter isReady (unTaskQueue currTaskQueue)

          getFinishedTasks = do
              SimulationState { currTime } <- get
              cpuFlows <- getCpuFlows
              return . S.fromList $ map (cpuTaskId . snd) $ concatMap (`within` (0, currTime)) cpuFlows

          getFreeNodes = do
              SimulationState { currSimulation } <- get
              filterM isFree (Map.keys currSimulation)

          isFree node = do
              SimulationState { currSimulation, currTime } <- get
              let flow = cpuFlow . fromJust $ Map.lookup node currSimulation
              return . null $ flow `within` (currTime, maxBound)

          findBestNode task = do
              SimulationState { taskNode } <- get
              --let taskTime = fromJust $ lab taskGraph task
              let findNode tid = fromJust $ Map.lookup tid taskNode
              let parents = map (first findNode) (lpre taskGraph task)
              freeNodes <- getFreeNodes
              return $ minimumBy (compare `on` tranferCost parents) freeNodes

          tranferCost :: [(NodeID, Int)] -> NodeID -> Int
          tranferCost parents node = sum $ map costFrom parents
              where costFrom (pNode, pWeight) = pWeight * length (esp pNode node systemGraph)

          rmTask task =
              modify $ \ st@(SimulationState { currTaskQueue = tq }) ->
                  st { currTaskQueue = TaskQueue $ task `delete` unTaskQueue tq }

          getTransfers task node = do
              SimulationState { taskNode } <- get
              let findNode tid    = fromJust $ Map.lookup tid taskNode
              let parents         = pre taskGraph task
              let transfer parent = Transfer (fromJust $ lab taskGraph parent)
                                             (esp (findNode parent) node systemGraph)
              return $ map transfer parents

          assignTask task = do
              node <- findBestNode task
              transfers <- getTransfers task node
              -- TODO
              rmTask task

          step = whileM_ (not <$> isFinished) $ do
              newTime    <- getNewTime
              updateTime newTime
              readyTasks <- getReadyTasks
              mapM_ assignTask readyTasks


genQueues :: Task -> StdGen -> [TaskQueue]
genQueues task rg = map (taskQueue task) [DiffQueue, CritPathQueue, RandomQueue rg]

taskQueue :: Task -> QueueGen -> TaskQueue
taskQueue task DiffQueue = TaskQueue $ sortBy (compare `on` diff) (nodes task)
    where diff n = lastTime task n - earlyTime task n
taskQueue task CritPathQueue = TaskQueue $ sortBy cmpCrits (nodes task)
    where cmpCrits n1 n2 = if nCrit1 == nCrit2
                               then (compare `on` getWeight task) n2 n1
                               else compare nCrit1 nCrit2
              where nCrit1 = nCrit rtask n1
                    nCrit2 = nCrit rtask n2
                    rtask  = grev task
taskQueue task (RandomQueue rg) = TaskQueue $ shuffle' ns (length ns) rg
    where ns = nodes task


nodeQueue :: System -> NodeQueue
nodeQueue system = NodeQueue $ sortBy (compare `on` length . neighbors system)
                                      (nodes system)

nCrit :: Task -> Node -> Int
nCrit task node = maximum $ map length allPaths
    where allPaths = pathTree (match node task)

tCrit :: Task -> Node -> Int
tCrit task node = maximum $ map time allPaths
    where allPaths = pathTree (match node task)
          time path = sum $ map (getWeight task) path

earlyTime :: Task -> Node -> Int
earlyTime task node = tCrit (grev task) node - getWeight task node

lastTime :: Task -> Node -> Int
lastTime task node = tCritGlobal - tCrit task node
    where tCritGlobal = maximum $ map (tCrit task) (nodes task)

getWeight :: DynGraph gr => gr a b -> Node -> a
getWeight gr node = fromJust $ lab gr node
