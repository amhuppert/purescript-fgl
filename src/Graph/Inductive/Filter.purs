module Graph.Inductive.Filter
  ( filterNodes
  , filterNodesOnLabel
  , filterLNodes
  , subgraph
  , filterEdges
  , filterEdgesOnLabel
  ) where


import Prelude

import Data.Foldable (class Foldable)
import Graph.Inductive.Class (class DynGraph, (&))
import Graph.Inductive.Class (empty, labNodes) as Graph
import Graph.Inductive.Core (delNodes, fold) as Graph
import Graph.Inductive.Types (Context(..), Edge(..), IncidentEdge(..), LEdge(..), LNode)
import Graph.Inductive.Types.Accessors as A
import Data.List as List
import Data.Set as Set

-- | Get a new graph containing only nodes that satisfy the predicate.
filterNodes :: forall gr k a b. DynGraph gr => Ord k
            => (k -> Boolean)
            -> gr k a b
            -> gr k a b
filterNodes p = filterLNodes (p <<< A.nodeFromLNode)

-- | Get a new graph containing only nodes whose label satisfies the predicate.
filterNodesOnLabel :: forall gr k a b. DynGraph gr => Ord k
            => (a -> Boolean)
            -> gr k a b
            -> gr k a b
filterNodesOnLabel p = filterLNodes (p <<< A.labelFromLNode)

-- | Get a new graph containing only nodes that satisfy the predicate.
filterLNodes :: forall gr k a b. DynGraph gr => Ord k
             => (LNode k a -> Boolean)
             -> gr k a b
             -> gr k a b
filterLNodes p gr =
  let lnodesToDelete = List.filter (not <<< p) $ Graph.labNodes gr
      nodesToDelete = map A.nodeFromLNode lnodesToDelete
   in Graph.delNodes nodesToDelete gr

-- | Returns the subgraph induced by the supplied nodes
subgraph :: forall gr k a b f. Ord k => DynGraph gr => Foldable f
         => f k
         -> gr k a b
         -> gr k a b
subgraph vs = let vs'= Set.fromFoldable vs
               in filterNodes (_ `Set.member` vs')

-- | Get a new graph containing only edges that satisfy the predicate.
filterEdges :: forall gr k a b. Ord k => DynGraph gr => (LEdge k b -> Boolean) -> gr k a b -> gr k a b
filterEdges p = Graph.fold cfilter Graph.empty
  where cfilter (Context c@{ incomers, outgoers }) g =
          let newContext = Context (c { incomers = incomers'
                                      , outgoers = outgoers'
                                      }
                                   )
           in newContext & g
          where incomers' = List.filter (\(IncidentEdge {node,label}) ->
                                          let le = LEdge { edge: Edge { from: node, to: c.node}, label }
                                           in p le
                                        ) incomers
                outgoers' = List.filter (\(IncidentEdge {node: to, label}) ->
                                          let le = LEdge { edge: Edge { from: c.node, to }, label }
                                          in p le
                                        ) outgoers

-- | Get a new graph containing only edges whose label satisfies the predicate.
filterEdgesOnLabel :: forall gr k a b. Ord k => DynGraph gr => (b -> Boolean) -> gr k a b -> gr k a b
filterEdgesOnLabel p = filterEdges (p <<< A.labelFromLEdge)
