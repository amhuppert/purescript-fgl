module Graph.Inductive.Inspect.Context
   ( neighbors
   , successors
   , predecessors
   , outgoers
   , incomers
   , degree
   , outDegree
   , inDegree
   ) where

import Prelude

import Graph.Inductive.Types (Context(..), ContextRec, Edge(..), IncidentEdge(..), LEdge(..))
import Graph.Inductive.Types.Accessors as A
import Data.List (List)
import Data.List as List

-- | All 'Node's linked to or from in a 'Context'.
neighbors :: forall k a b. Eq k => Context k a b -> List k
neighbors c =
  let ns = successors' c <> predecessors' c
   in List.nub ns

-- | All 'Node's linked to in a 'Context'.
successors :: forall k a b. Eq k => Context k a b -> List k
successors = List.nub <<< successors'

successors' :: forall k a b. Context k a b -> List k
successors' = map A.nodeFromIncidentEdge <<< A.outgoersFromContext

-- | All 'Node's linked from in a 'Context'.
predecessors :: forall k a b. Eq k => Context k a b -> List k
predecessors = List.nub <<< predecessors'

predecessors' :: forall k a b. Context k a b -> List k
predecessors' = map A.nodeFromIncidentEdge <<< A.incomersFromContext

-- | All outward-directed 'LEdge's in a 'Context'.
outgoers :: forall k a b. Context k a b -> List (LEdge k b)
outgoers (Context c) = map (incidentEdgeToLEdge c) c.outgoers

incidentEdgeToLEdge :: forall k a b. ContextRec k a b -> IncidentEdge k b -> LEdge k b
incidentEdgeToLEdge c (IncidentEdge { label, node: to }) =
  let edge = Edge { from: c.node, to }
  in LEdge { edge, label }

-- | All inward-directed 'LEdge's in a 'Context'.
incomers :: forall k a b. Context k a b -> List (LEdge k b)
incomers (Context c) = map (incidentEdgeToLEdge c) c.incomers

-- | The outward degree of a 'Context'.
outDegree :: forall k a b. Ord k => Context k a b -> Int
outDegree = List.length <<< A.outgoersFromContext

-- | The inward degree of a 'Context'.
inDegree :: forall k a b. Ord k => Context k a b -> Int
inDegree = List.length <<< A.incomersFromContext

-- | The degree of a 'Context'.
degree :: forall k a b. Context k a b -> Int
degree (Context c) = List.length c.incomers + List.length c.outgoers
