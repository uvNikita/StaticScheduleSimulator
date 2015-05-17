{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}

module Shedule (
      genQueues
    , Task
    , System
    , simulate
    , SimulationConfig (..)
    , SimulationType (..)
    , ConnectionType (..)
    , QueueGen (..)
) where

import           Data.Foldable                   (any, concatMap, foldlM)
import           Data.Function                   (on)
import           Data.Graph.Analysis             (pathTree)
import           Data.Graph.Inductive            (DynGraph, Graph, Node, esp,
                                                  grev, lab, lpre, match,
                                                  neighbors, nodes, pre)
import           Data.IntervalMap.Generic.Strict (Interval, IntervalMap,
                                                  lowerBound, rightClosed,
                                                  upperBound, within, intersecting)
import qualified Data.IntervalMap.Generic.Strict as IMap
import           Data.List                       (delete, find, minimumBy,
                                                  sortBy, intercalate)
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (catMaybes, fromJust)
import           Data.Sequence                   (Seq)
import qualified Data.Sequence                   as Seq
import           Data.Set                        (member)
import qualified Data.Set                        as S
import           Prelude                         hiding (any, concatMap)



import           System.Random                   (StdGen)
import           System.Random.Shuffle           (shuffle')

import           Control.Applicative             ((<$>), (<*>))
import           Control.Arrow                   (first)
import           Control.Monad                   (filterM)
import           Control.Monad.Loops             (whileM_)
import           Control.Monad.State.Strict      (State, execState, get, modify)

import           Graph                           (Directed, Undirected)

newtype TaskQueue = TaskQueue { unTaskQueue :: [Node] } deriving (Eq, Show)
newtype NodeQueue = NodeQueue { unNodeQueue :: Seq Node } deriving (Eq, Show)

type TaskID = Int
type NodeID = Int

type NodeSpeed       = Int
type ConnectionSpeed = Int

type TaskWeight     = Int
type TransferWeight = Int

type Task   = Directed   TaskWeight TransferWeight
type System = Undirected NodeSpeed  ConnectionSpeed

data QueueGen = DiffQueue | CritPathQueue | RandomQueue StdGen

newtype Simulation = Simulation { unSimulation :: Map Node NodeFlow }

instance Show Simulation where
    show (Simulation sim) = intercalate "\n" (map show $ Map.toList sim)

data SimulationConfig = SimulationConfig { linksCount     :: Int
                                         , connectionType :: ConnectionType
                                         , queueGen       :: QueueGen
                                         , simulationType :: SimulationType }
data SimulationType = WithPreTransfers | WithoutPreTransfers

data ConnectionType = FullDuplex | HalfDuplex

data NodeFlow = NodeFlow { cpuFlow :: CPUFlow, linkFlows :: Seq LinkFlow } deriving (Show)

type CPUFlow = Flow CPUFlowInfo
type LinkFlow = Flow LinkFlowInfo

type Flow a = IntervalMap (Ticks, Ticks) a

type Ticks = Int

data CPUFlowInfo  = CPUFlowInfo  { cpuTaskId :: TaskID } deriving (Show, Eq)

data LinkFlowInfo = LinkFlowInfo { linkFromId :: TaskID
                                 , linkToId   :: TaskID
                                 , linkNodeId :: NodeID } deriving (Show)

isLinkTo :: NodeID -> LinkFlowInfo -> Bool
isLinkTo to (LinkFlowInfo {..}) = to == linkNodeId

data SimulationState = SimulationState { currTime       :: Ticks
                                       , currTaskQueue  :: TaskQueue
                                       , currSimulation :: Map Node NodeFlow
                                       , taskNode       :: Map TaskID NodeID }

data Transfer = Transfer TaskID TaskWeight [NodeID]

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
          emptyNodeFlow = NodeFlow emptyFlow (Seq.replicate linksCount emptyFlow)
          emptyFlow = IMap.empty
          isFinished = null . unTaskQueue . currTaskQueue <$> get
          updateTime newTime = modify (\ st -> st { currTime = newTime })

          getCpuFlows = do
              SimulationState { currSimulation } <- get
              return $ map cpuFlow (Map.elems currSimulation)

          getNewTime = do
              SimulationState { currTime } <- get
              cpuFlows <- getCpuFlows
              let futureTasks = map (`intersecting` (currTime, maxBound)) cpuFlows
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
              return . null $ flow `intersecting` (currTime, maxBound)

          findBestNode task = do
              SimulationState { taskNode } <- get
              --let taskTime = fromJust $ lab taskGraph task
              let findNode tid = fromJust $ Map.lookup tid taskNode
              let parents      = map (first findNode) (lpre taskGraph task)
              freeNodes <- getFreeNodes
              return $ if null freeNodes
                         then Nothing
                         else Just $ minimumBy (compareByTranferCost parents) freeNodes

          compareByTranferCost :: [(NodeID, Int)] -> NodeID -> NodeID -> Ordering
          compareByTranferCost parents n1 n2 =
              case (compare `on` tranferCost parents) n1 n2 of
                  EQ  -> (compare `on` flip Seq.elemIndexL (unNodeQueue nq)) n1 n2
                  cmp -> cmp

          tranferCost :: [(NodeID, Int)] -> NodeID -> Int
          tranferCost parents node = sum $ map costFrom parents
              where costFrom (pNode, pWeight) = pWeight * length (esp pNode node systemGraph)

          rmTask task =
              modify $ \ st@(SimulationState { currTaskQueue }) ->
                  st { currTaskQueue = TaskQueue $ task `delete` unTaskQueue currTaskQueue }

          getTransfers task node = do
              SimulationState { taskNode } <- get
              let findNode tid    = fromJust $ Map.lookup tid taskNode
              let parents         = lpre taskGraph task
              let transfer (pid, weight) = Transfer pid
                                                    weight
                                                    (esp (findNode pid) node systemGraph)
              return $ map transfer parents

          send node link time info =
              modify $ \ st @ (SimulationState { currSimulation }) ->
                  st { currSimulation = Map.adjust modifyNodeFlow node currSimulation }
              where modifyNodeFlow (NodeFlow cf lf) = NodeFlow cf $ Seq.adjust modifyLinkFlow link lf
                    modifyLinkFlow = IMap.insert time info

          doSend :: TaskID -> TaskID -> Int -> Ticks -> (NodeID, NodeID) -> State SimulationState Ticks
          doSend source target weight start (from, to) = do
            SimulationState { currSimulation } <- get
            let end = start + weight
            let NodeFlow _ linkFlowsFrom = currSimulation Map.! from
            let NodeFlow _ linkFlowsTo   = currSimulation Map.! to
            let isSending toNode = any (isLinkTo toNode) . map snd . concatMap (`IMap.intersecting` (start, end))
            let findLinks st = if checkChannel
                                   then (st,,) <$> Seq.findIndexL isFreeLink linkFlowsFrom
                                               <*> Seq.findIndexL isFreeLink linkFlowsTo
                                   else Nothing
                                   where checkChannel = case connectionType of
                                                            HalfDuplex -> checkFrom && checkTo
                                                            FullDuplex -> checkFrom
                                         checkFrom = not  $ isSending to   linkFlowsFrom
                                         checkTo   = not  $ isSending from linkFlowsTo
                                         isFreeLink flow = null $ IMap.intersecting flow (start, end)

            let (st, lf, _) = head . catMaybes $ map findLinks [start..] -- ignoring link to. Maybe wrong
            send from lf (st, st + weight) $ LinkFlowInfo source target to
            return $ st + weight

          calcStartTime source = do
              SimulationState { taskNode, currSimulation } <- get
              let node = fromJust $ Map.lookup source taskNode
              let nodeFlow = fromJust $ Map.lookup node currSimulation
              let ((_, end), _) = fromJust . find ((== CPUFlowInfo source) . snd) $ IMap.assocs (cpuFlow nodeFlow)
              return end


          doTransfer :: TaskID -> Transfer -> State SimulationState Ticks
          doTransfer target (Transfer source weight path) = do
              SimulationState { currTime } <- get
              startTime <- case simulationType of
                               WithoutPreTransfers -> return currTime
                               WithPreTransfers    -> calcStartTime source
              foldlM (doSend source target weight) startTime (zip path (tail path))

          assignCalculation node task time =
              modify $ \ st @ ( SimulationState { currSimulation, taskNode } ) ->
                      st { taskNode       = Map.insert task node taskNode
                         , currSimulation = Map.adjust modifyNodeFlow node currSimulation }
              where calc = fromJust $ lab taskGraph task
                    modifyNodeFlow (NodeFlow cf lfs) = NodeFlow newCPUFlow lfs
                        where newCPUFlow = IMap.insert (time, time + calc) (CPUFlowInfo task) cf

          assignTask task = do
              maybeNode <- findBestNode task
              case maybeNode of
                  Nothing -> return ()
                  Just node -> do
                      SimulationState { currTime } <- get
                      transfers <- getTransfers task node
                      times     <- mapM (doTransfer task) transfers
                      let time = if null transfers then currTime else maximum times
                      assignCalculation node task time
                      rmTask task

          step = whileM_ (not <$> isFinished) $ do
              readyTasks <- getReadyTasks
              mapM_ assignTask readyTasks
              newTime    <- getNewTime
              updateTime newTime


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
nodeQueue system = NodeQueue . Seq.reverse $ Seq.sortBy (compare `on` length . neighbors system)
                                                        (Seq.fromList $ nodes system)

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
