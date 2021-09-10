module Main where

import Prelude

import Control.Comonad
import Data.List (List(..))
import Effect (Effect)
import Effect.Console (logShow)
import Graph.Inductive.Class (match, mkGraph)
import Graph.Inductive.Core (labelEdge, labelNode)
import Graph.Inductive.Impl (Gr)
import Graph.Inductive.Types (Context(..), Edge(..))
import PointedGraph (PointedGraph(..), refocus)

g :: Gr Int String Unit 
g = mkGraph [ labelNode 1 "A"
            , labelNode 2 "B"
            ]
            [ labelEdge (Edge {from: 1, to: 2}) unit
            , labelEdge (Edge {from: 1, to: 2}) unit
            , labelEdge (Edge {from: 2, to: 1}) unit
            ]

p :: PointedGraph Gr Int Unit String
p = PointedGraph (
  Context 
    { incomers : Nil
    , node : 0
    , label : "0"
    , outgoers : Nil
    }) g


main :: Effect Unit
main = do
  logShow g
  logShow $ match 1 g
  -- comonad
  logShow $ extract p
  logShow $ extract (refocus p 1)
