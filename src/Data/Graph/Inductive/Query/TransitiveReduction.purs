module Data.Graph.Inductive.Query.TransitiveReduction
       (transitiveReduction
       ) where

import Prelude

import Data.Graph.Inductive.Core (class DynGraph, Context)
import Data.Graph.Inductive.Core as Graph
import Data.Graph.Inductive.Query.DFS as DFS
import Data.List as List

-- | <https://en.wikipedia.org/wiki/Transitive_reduction Transitive Reduction>
-- O(|V|*|E|)
transitiveReduction :: forall gr a b. DynGraph gr => gr a b -> gr a b
transitiveReduction graph = Graph.fold reduceV graph graph

  where reduceV :: Context a b -> gr a b -> gr a b
        reduceV curr g =
          let toDel =
                List.concatMap
                  (reachableFromSucc >>> map (mkEdgeFrom curr.node))
                  (List.fromFoldable $ Graph.suc' curr)
           in Graph.delEdges toDel g

          where reachableFromSucc s = List.filter (_ /= s) $ DFS.reachable s graph
                mkEdgeFrom from to = { from, to }
