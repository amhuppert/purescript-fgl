module Graph.Inductive.Algorithms.TransitiveReduction
       (transitiveReduction
       ) where

import Prelude

import Data.Foldable (foldr)
import Data.Lazy as Lazy
import Data.List as StrictList
import Data.List.Lazy as List
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Graph.Inductive.Algorithms.DFS as DFS
import Graph.Inductive.Class (class DynGraph)
import Graph.Inductive.Class (match, merge) as Graph
import Graph.Inductive.Inspect (nodes) as Graph
import Graph.Inductive.Inspect.Context as Context
import Graph.Inductive.Types (Context(..), Edge(..), GraphDecomposition(..), IncidentEdge(..))

-- | <https://en.wikipedia.org/wiki/Transitive_reduction Transitive Reduction>
-- |
-- | O(|V|*|E|)
transitiveReduction :: forall gr k a b. Ord k => DynGraph gr => gr k a b -> gr k a b
transitiveReduction graph = foldr reduceV graph $ Graph.nodes graph

  where
    reduceV :: k -> gr k a b -> gr k a b
    reduceV v g =
      case Graph.match v g of
        Just (Decomp {context: (Context c), remaining}) ->
          let edgesToDelete =
                Set.fromFoldable $
                List.concatMap
                  (reachable >>> map (mkEdgeFrom v))
                  (List.fromFoldable $ Context.successors (Context c))
              c' = Context $
                    c { outgoers =
                          StrictList.filter (\(IncidentEdge ie) ->
                                              not $ mkEdgeFrom v ie.node `Set.member` edgesToDelete
                                            ) c.outgoers
                      }
          in Graph.merge c' (Lazy.force remaining)
        Nothing -> g

    reachable s = List.filter (_ /= s) $ DFS.reachable s graph
    mkEdgeFrom from to = Edge { from, to }
