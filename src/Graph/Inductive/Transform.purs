module Graph.Inductive.Transform where

import Prelude
import Data.List as List
import Data.Newtype (over)
import Graph.Inductive.Class (class DynGraph)
import Graph.Inductive.Class as Graph
import Graph.Inductive.Core as Core
import Graph.Inductive.Types (Context(..), IncidentEdge(..), UGraph)

mapContexts :: forall gr k a b c d. Ord k => DynGraph gr => (Context k a b -> Context k c d) -> gr k a b -> gr k c d
mapContexts f = Core.fold (\c gr -> Graph.merge (f c) gr) Graph.empty

mapNodes :: forall gr k a b a'. Ord k => DynGraph gr => (a -> a') -> gr k a b -> gr k a' b
mapNodes f = Graph.mapNodesWithKey f'
  where f' _ label = f label

mapEdges :: forall gr k a b b'. Ord k => DynGraph gr => (b -> b') -> gr k a b -> gr k a b'
mapEdges f = mapContexts (over Context updateContextRec)
  where updateContextRec c = c { incomers = updateIncEdges c.incomers
                               , outgoers = updateIncEdges c.outgoers
                               }
        updateIncEdges = map updateLabel
        updateLabel (IncidentEdge {node,label}) = IncidentEdge {node, label: f label}

mapNodesEdges :: forall gr k a b a' b'. Ord k => DynGraph gr => (a -> a') -> (b -> b') -> gr k a b -> gr k a' b'
mapNodesEdges nodeMapper edgeMapper = mapContexts (over Context updateContextRec)
  where updateContextRec c = c { incomers = updateIncEdges c.incomers
                               , outgoers = updateIncEdges c.outgoers
                               , label = nodeMapper c.label
                               }
        updateIncEdges = map updateELabel
        updateELabel (IncidentEdge {node,label}) = IncidentEdge {node, label: edgeMapper label}

modifyNodeLabel :: forall gr k a b. Ord k => DynGraph gr => (a -> a) -> k -> gr k a b -> gr k a b
modifyNodeLabel = Graph.mapNode

-- | Reverse the direction of all edges.
reverseEdges :: forall gr k a b. Ord k => DynGraph gr => gr k a b -> gr k a b
reverseEdges = mapContexts (\(Context c) -> Context $ c { incomers = c.outgoers, outgoers = c.incomers })

-- | Make the graph undirected, i.e. for every edge from A to B, there
-- exists an edge from B to A.
mkUndirected :: forall gr k a b. Ord k => Eq b => DynGraph gr => gr k a b -> gr k a b
mkUndirected = mapContexts f
  where f (Context c) = let ps = List.nub (c.incomers <> c.outgoers)
                            c' = c { incomers = ps, outgoers = ps }
                         in Context c'

-- | Remove all labels.
unlabelGraph :: forall gr k a b. Ord k => DynGraph gr => gr k a b -> UGraph gr k
unlabelGraph = mapContexts (\(Context c) -> Context $
                                            c { incomers = unlabAdj c.incomers
                                              , label = unit
                                              , outgoers = unlabAdj c.outgoers
                                              }
                           )
  where unlabAdj = map (\(IncidentEdge {node}) -> IncidentEdge { node, label: unit })
