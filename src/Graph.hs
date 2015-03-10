{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graph (
      Directed
    , Undirected
    , Node
) where

import           Data.Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.Graph.Inductive as G
import           Data.Graph.Inductive (undir, Graph, DynGraph, LNode, LEdge)

newtype Node a = Node { unNode :: LNode a }
newtype Edge b = Edge { unEdge :: LEdge b }

newtype Directed   a b = Directed   (G.Gr a b) deriving (Show, Graph, DynGraph)
newtype Undirected a b = Undirected (G.Gr a b) deriving (Show, Graph, DynGraph)

instance FromJSON (Node Int) where
    parseJSON (Object v) = do
        idx    <- v .: "idx"
        weight <- v .: "weight"

        return $ Node (idx, weight)
    parseJSON _          = fail "node expected to be an object"

instance FromJSON (Edge Int) where
    parseJSON (Object v) = do
        from    <- v .: "from"
        to      <- v .: "to"
        weight  <- v .: "weight"

        return $ Edge (from, to, weight)
    parseJSON _          = fail "edge expected to be an object"

parseGraph :: DynGraph g => Value -> Parser (g Int Int)
parseGraph (Object v) = do
    nodes    <- v .: "nodes"
    edges    <- v .: "edges"
    return $ G.mkGraph (map unNode nodes :: [LNode Int]) (map unEdge edges :: [LEdge Int])
parseGraph _          = fail "node expected to be an object"

instance FromJSON (Directed Int Int) where
    parseJSON = parseGraph

instance FromJSON (Undirected Int Int) where
    parseJSON o = undir `fmap` parseGraph o
