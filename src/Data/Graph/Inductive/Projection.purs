module Data.Graph.Inductive.Projection where

import Prelude

import Data.Graph.Inductive.Core (class Graph, Node, Edge)
import Data.Graph.Inductive.Core as Core
import Data.List (List)
import Data.List as List
import Data.Maybe (fromJust, isJust)
import Partial.Unsafe (unsafePartial)

nodes :: forall gr a b. Graph gr => gr a b -> Array Node
nodes = Core.labNodes >>> map Core.unlabelNode

edges :: forall gr a b. Graph gr => gr a b -> Array Edge
edges = Core.labEdges >>> map Core.unlabelEdge

newNodes :: forall gr a b. Graph gr => Int -> gr a b -> List Node
newNodes count graph
  | Core.isEmpty graph = 0 List...  (count - 1)
  | otherwise = let { max } = unsafePartial (fromJust (Core.nodeRange graph))
                 in (max + 1) List... (max + count)

-- | True if the Node is present in the Graph
elem :: forall gr a b. Graph gr => Node -> gr a b -> Boolean
elem n = Core.match n >>> isJust
