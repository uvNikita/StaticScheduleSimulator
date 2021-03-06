{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Graph (
      Directed
    , Undirected
    , Validator (..)
    , Error
    , validate
    , validateOne
    , eitherDecode
    , generate
) where

import           Data.Aeson            (FromJSON, ToJSON, Value (..), object,
                                        parseJSON, toJSON, (.:), (.=))
import qualified Data.Aeson            as A
import           Data.Aeson.Types      (Parser)
import           Data.ByteString.Lazy  (ByteString)
import           Data.Function         (on)
import           Data.Graph.Analysis   (cyclesIn', rootsOf')
import           Data.Graph.Inductive  (DynGraph, Gr, Graph, isConnected,
                                        isEmpty, lab, labEdges, leveln, mkGraph,
                                        undir)
import           Data.List             (groupBy, inits)
import           Data.Maybe            (fromJust, mapMaybe)
import           Data.Text             (Text)
import qualified Data.Text             as T

import           Control.Applicative   ((<$>), (<*>))
import           Control.Arrow         ((&&&))
import           System.Random         (Random, RandomGen, randomRs)
import qualified System.Random         as R
import           System.Random.Shuffle (shuffle')


data Node = Node { nodeId     :: Int
                 , nodeWeight :: Int
                 , nodeX      :: Int
                 , nodeY      :: Int }

data Edge = Edge { edgeId     :: Int
                 , edgeFrom   :: Int
                 , edgeTo     :: Int
                 , edgeWeight :: Int }

newtype Directed   a b = Directed   (Gr a b) deriving (Show, Graph, DynGraph)
newtype Undirected a b = Undirected (Gr a b) deriving (Show, Graph, DynGraph)

data Validator = DAG | Connected | NonEmpty

vError :: Validator -> Error
vError DAG       = "not acyclic"
vError Connected = "not fully connected"
vError NonEmpty  = "empty graph"

type Error = Text

validate :: DynGraph gr => gr a b -> [Validator] -> [Error]
validate gr = mapMaybe (validateOne gr)

validateOne :: DynGraph gr => gr a b -> Validator -> Maybe Error
validateOne graph validator = if pass validator graph
                                  then Nothing
                                  else Just $ vError validator

pass :: DynGraph gr => Validator -> gr a b -> Bool
pass DAG = null . cyclesIn'
pass Connected = isConnected
pass NonEmpty  = not . isEmpty

generate
    :: ( Integral a , Integral b, Random a , Random b
       , RealFrac c , Graph gr , RandomGen gen) =>
       (a, a)   -- ^ Range of node weights
    -> (b, b)   -- ^ Range of edge weights
    -> Int      -- ^ The number of nodes
    -> c        -- ^ Required correlation (Wn / (Wn + We))
    -> gen      -- ^ Random generator
    -> gr a b   -- ^ Resulting random graph
generate nodeWeightRange
         (eMinParam, eMaxParam)
         nodeCount
         correlation
         rGen = mkGraph nodes edges
    where
        nodes = zip [1..] $ take nodeCount nodesStream
        nodesWeight = fromIntegral . sum . map snd $ nodes
        edgesWeight = round $ nodesWeight / correlation - nodesWeight

        edgesCount = nodeCount * (nodeCount - 1) `div` 2

        edgeWeightRange = if eMinCalc > eMinParam
                              then (eMinCalc, eMaxParam + (eMinCalc - eMinParam))
                              else (eMinParam, eMaxParam)
            where eMinCalc = edgesWeight `div` fromIntegral edgesCount

        (egen', ngen) = R.split rGen
        (egen, sgen)  = R.split egen'
        nodesStream = randomRs nodeWeightRange egen
        edgesStream = randomRs edgeWeightRange ngen

        edgesWeights' = last
                      . takeWhile ((< edgesWeight) . sum)
                      . inits $ take (edgesCount - 1) edgesStream
        lastEdgeWeight = edgesWeight - sum edgesWeights'
        edgesWeights = lastEdgeWeight : edgesWeights'

        connections = shuffle' [(f, t) | f <- [1 .. nodeCount],
                                         t <- [1 .. nodeCount],
                                         f < t]
                               edgesCount
                               sgen
        edges = zipWith (\ (f, t) w -> (f, t, w)) connections edgesWeights


eitherDecode :: FromJSON b => ByteString -> Either Text b
eitherDecode s = case A.eitherDecode s of
                     Left e -> Left $ T.pack e
                     Right v -> Right v

instance FromJSON Node where
    parseJSON (Object v) = Node
                       <$> v .: "idx"
                       <*> v .: "weight"
                       <*> v .: "x"
                       <*> v .: "y"
    parseJSON _          = fail "node expected to be an object"

instance ToJSON Node where
    toJSON Node {..} = object [ "idx"    .= nodeId
                              , "weight" .= nodeWeight
                              , "x"      .= nodeX
                              , "y"      .= nodeY]

instance FromJSON Edge where
    parseJSON (Object v) = Edge
                       <$> v .: "idx"
                       <*> v .: "from"
                       <*> v .: "to"
                       <*> v .: "weight"
    parseJSON _          = fail "edge expected to be an object"

instance ToJSON Edge where
    toJSON Edge {..} = object [ "idx"    .= edgeId
                              , "from"   .= edgeFrom
                              , "to"     .= edgeTo
                              , "weight" .= edgeWeight]

parseGraph :: DynGraph g => Value -> Parser (g Int Int)
parseGraph (Object v) = do
    let toLNode Node {..} = (nodeId, nodeWeight)
    let toLEdge Edge {..} = (edgeFrom, edgeTo, edgeWeight)
    nodes    <- v .: "nodes"
    edges    <- v .: "edges"
    return $ mkGraph (map toLNode nodes) (map toLEdge edges)
parseGraph _          = fail "node expected to be an object"

instance FromJSON (Directed Int Int) where
    parseJSON = parseGraph

instance FromJSON (Undirected Int Int) where
    parseJSON o = undir `fmap` parseGraph o

instance ToJSON (Directed Int Int) where
    toJSON (Directed gr) = object ["nodes" .= nodes, "edges" .= edges]
        where nodes = concatMap fromLevel levels
              edges = zipWith toEdge [1..] (labEdges gr)
              toEdge idx (f, t, w) = Edge { edgeId = idx, edgeFrom = f, edgeTo = t, edgeWeight = w }
              levels = groupper . groupBy ((==) `on` snd) $ lvls
                      where roots = rootsOf' gr
                            lvls  = leveln (zip roots [0, 0 ..]) gr
                            groupper = map ((snd . head) &&& map fst)
              fromLevel (lvl, ns) = zipWith toNode ns [0..]
                  where toNode idx shift = Node idx (fromJust $ lab gr idx) (shift * 90 + 40) (lvl * 180 + 40)
