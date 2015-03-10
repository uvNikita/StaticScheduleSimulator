{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Graph (
      Directed
    , Undirected
    , Node
) where

import Data.Aeson
import qualified Data.IGraph as G
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Control.Monad (forM)


data Node = Node { nodeId :: String
                 , nodeWeight :: Int } deriving (Show, Eq, Generic)

data EdgeJS = EdgeJS { fromNode :: String
                     , toNode :: String
                     , edgeWeight :: Int } deriving (Show)

instance Hashable Node

type Directed = G.Graph (G.Weighted G.D) Node
type Undirected = G.Graph (G.Weighted G.U) Node

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
    let nodesMap = map (\ n -> (nodeId n, n)) nodes
    edges    <- v .: "edges"
    let getNode idx = maybe (fail $ "can't find node: " ++ show idx)
                            return
                            (lookup idx nodesMap)
    ls <- forM edges $ \ EdgeJS { fromNode, toNode, edgeWeight } -> do
        from <- getNode fromNode
        to   <- getNode toNode
        return (from, to, edgeWeight)

    return $ G.fromListWeighted ls
parseGraph _          = fail "node expected to be an object"

instance FromJSON (Directed) where
    parseJSON = parseGraph

instance FromJSON (Undirected) where
    parseJSON = parseGraph
