module Graph.Inductive.Algorithms.TransitiveReduction
       (transitiveReduction
       ) where

import Prelude

import Graph.Inductive.Class (class DynGraph)
import Graph.Inductive.Core as Graph
import Graph.Inductive.Inspect.Context as Context
import Graph.Inductive.Algorithms.DFS as DFS
import Graph.Inductive.Types (Context(..), Edge(..))
import Data.List as List

-- | <https://en.wikipedia.org/wiki/Transitive_reduction Transitive Reduction>
-- |
-- | O(|V|*|E|)
transitiveReduction :: forall gr k a b. Ord k => DynGraph gr => gr k a b -> gr k a b
transitiveReduction graph = Graph.fold reduceV graph graph

  where reduceV :: Context k a b -> gr k a b -> gr k a b
        reduceV (Context curr) g =
          let toDel =
                List.concatMap
                  (reachableFromSucc >>> map (mkEdgeFrom curr.node))
                  (Context.successors (Context curr))
           in Graph.delEdges toDel g

          where reachableFromSucc s = List.filter (_ /= s) $ DFS.reachable s graph
                mkEdgeFrom from to = Edge { from, to }
