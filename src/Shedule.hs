module Shedule (
      genQueues
    , Task
    , System
) where

import System.Random (StdGen)
import System.Random.Shuffle (shuffle')
import Data.List (sortBy)
import Data.Maybe (fromJust)
import Data.Function (on)
import Data.Graph.Inductive (Graph, Node, nodes, match, lab, grev, DynGraph)
import Data.Graph.Analysis (pathTree)

import Graph (Directed, Undirected)

type Queue = [Node]

type Task   = Directed   Int Int
type System = Undirected Int Int

data QueueGen = DiffQueue | CritPathQueue | RandomQueue StdGen

genQueues :: Task -> StdGen -> [Queue]
genQueues task rg = map (queue task) [DiffQueue, CritPathQueue, RandomQueue rg]


queue :: Task -> QueueGen -> [Node]
queue task DiffQueue = sortBy (compare `on` diff) (nodes task)
    where diff n = lastTime task n - earlyTime task n
queue task CritPathQueue = sortBy cmpCrits (nodes task)
    where cmpCrits n1 n2 = if nCrit1 == nCrit2
                               then (compare `on` getWeight task) n2 n1
                               else compare nCrit1 nCrit2
              where nCrit1 = nCrit rtask n1
                    nCrit2 = nCrit rtask n2
                    rtask  = grev task
queue task (RandomQueue rg) = shuffle' ns (length ns) rg
    where ns = nodes task


-- nCrits     :: Task -> [(Node, Int)]
-- tCrits     :: Task -> [(Node, Int)]
-- earlyTimes :: Task -> [(Node, Int)]
-- lastTimes  :: Task -> [(Node, Int)]
-- nCrits     = mapg nCrit
-- tCrits     = mapg tCrit
-- earlyTimes = mapg earlyTime
-- lastTimes  = mapg lastTime

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
lastTime  task node = tCritGlobal - tCrit task node
    where tCritGlobal = maximum $ map (tCrit task) (nodes task)

getWeight :: DynGraph gr => gr a b -> Node -> a
getWeight gr node = fromJust $ lab gr node

-- mapg :: Graph gr => (gr a b -> Node -> t) -> gr a b -> [(Node, t)]
-- mapg f gr = map (\ n -> (n, f gr n)) (nodes gr)
