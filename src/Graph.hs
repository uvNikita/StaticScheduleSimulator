{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graph (
      Directed
    , Undirected
    , Validator (..)
    , validate
    , validateOne
    , eitherDecode
    , Error
) where

import           Data.Maybe (mapMaybe)
import qualified Data.Aeson as A
import           Data.Aeson (FromJSON, parseJSON, Value(..), (.:))
import           Data.Aeson.Types (Parser)
import           Data.Graph.Inductive (Gr, mkGraph, isConnected, undir, Graph, DynGraph, LNode, LEdge)
import           Data.Graph.Analysis (cyclesIn')
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.ByteString.Lazy (ByteString)

newtype Node a = Node { unNode :: LNode a }
newtype Edge b = Edge { unEdge :: LEdge b }

newtype Directed   a b = Directed   (Gr a b) deriving (Show, Graph, DynGraph)
newtype Undirected a b = Undirected (Gr a b) deriving (Show, Graph, DynGraph)

data Validator = DAG | Connected

vError :: Validator -> Error
vError DAG       = "not acyclic"
vError Connected = "not fully connected"

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

eitherDecode :: FromJSON b => ByteString -> Either Text b
eitherDecode s = case A.eitherDecode s of
                     Left e -> Left $ T.pack e
                     Right v -> Right v

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
    return $ mkGraph (map unNode nodes :: [LNode Int]) (map unEdge edges :: [LEdge Int])
parseGraph _          = fail "node expected to be an object"

instance FromJSON (Directed Int Int) where
    parseJSON = parseGraph

instance FromJSON (Undirected Int Int) where
    parseJSON o = undir `fmap` parseGraph o
