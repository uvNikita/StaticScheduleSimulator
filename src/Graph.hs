{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graph (
      Directed
    , Undirected
    , Node
) where

import Data.Aeson
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive (undir, Graph, DynGraph)
import qualified Data.Graph.Inductive.NodeMap as NM
import GHC.Generics (Generic)
import Control.Monad (forM)
import Data.Function (on)


data Node = Node { nodeId :: String
                 , nodeWeight :: Int } deriving (Show, Generic)

instance Eq Node where
    (==) = (==) `on` nodeId

instance Ord Node where
    compare = compare `on` nodeId

data EdgeJS = EdgeJS { fromNode :: String
                     , toNode :: String
                     , edgeWeight :: Int } deriving (Show)

newtype DirectedG a b = Directed (G.Gr a b) deriving (Show, Graph, DynGraph)
newtype UndirectedG a b = Undirected (G.Gr a b) deriving (Show, Graph, DynGraph)
type Directed = DirectedG Node Int
type Undirected = UndirectedG Node Int

instance FromJSON (Node) where
    parseJSON (Object v) = do
        idx    <- v .: "idx"
        weight <- v .: "weight"

        return Node { nodeId = idx, nodeWeight = weight }
    parseJSON _          = fail "node expected to be an object"

instance FromJSON (EdgeJS) where
    parseJSON (Object v) = do
        from    <- v .: "from"
        to      <- v .: "to"
        weight  <- v .: "weight"

        return EdgeJS { fromNode = from, toNode = to, edgeWeight = weight }
    parseJSON _          = fail "edge expected to be an object"

parseGraph (Object v) = do
    nodes    <- v .: "nodes"
    edges    <- v .: "edges"
    let nodesMap = map (\ n -> (nodeId n, n)) nodes
    let getNode idx = maybe (fail $ "can't find node: " ++ show idx)
                            return
                            (lookup idx nodesMap)
    connections <- forM edges $ \ EdgeJS { fromNode, toNode, edgeWeight } -> do
        from <- getNode fromNode
        to   <- getNode toNode
        return (from, to, edgeWeight)

    return . fst $ NM.mkMapGraph nodes connections
parseGraph _          = fail "node expected to be an object"

instance FromJSON (Directed) where
    parseJSON = parseGraph

instance FromJSON (Undirected) where
    parseJSON o = undir `fmap` parseGraph o
