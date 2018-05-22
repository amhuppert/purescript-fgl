module Data.Graph.Inductive.Basic where

import Prelude

import Data.Array as Array
import Data.Graph.Inductive.Core (suc', class DynGraph, class Graph, Context, UGraph, fold, mapContexts)
import Data.Tuple (Tuple(..))

-- | Reverse the direction of all edges.
reverseEdges :: forall gr a b. DynGraph gr => gr a b -> gr a b
reverseEdges = mapContexts (\c -> c { incomers = c.outgoers, outgoers = c.incomers })

-- | Make the graph undirected, i.e. for every edge from A to B, there
-- exists an edge from B to A.
mkUndirected :: forall gr a b. Eq b => DynGraph gr => gr a b -> gr a b
mkUndirected = mapContexts f
  where f c = let ps = Array.nubBy adjEq (c.incomers <> c.outgoers)
               in c { incomers = ps, outgoers = ps }
        adjEq (Tuple l1 v1) (Tuple l2 v2) = v1 == v2 && l1 == l2

-- | Remove all labels.
unlabelGraph :: forall gr a b. DynGraph gr => gr a b -> UGraph gr
unlabelGraph = mapContexts (\c -> c { incomers = unlabAdj c.incomers
                                    , label = unit
                                    , outgoers = unlabAdj c.outgoers
                                    }
                           )
  where unlabAdj = map (\(Tuple _ v) -> Tuple unit v)

-- | Return all 'Context's for which the given function returns 'True'.
selectContexts :: forall gr a b. Graph gr => (Context a b -> Boolean) -> gr a b -> Array (Context a b)
selectContexts p = fold (\c cs -> if p c then cs `Array.snoc` c else cs) []


-- some predicates and classifications
--

-- | 'True' if the graph has any edges of the form (A, A).
hasLoop :: forall gr a b. Graph gr => gr a b -> Boolean
hasLoop = not <<< Array.null <<< selectContexts (\c -> c.node `Array.elem` suc' c)

-- | The inverse of 'hasLoop'.
isSimple :: forall gr a b. Graph gr => gr a b -> Boolean
isSimple = not <<< hasLoop
