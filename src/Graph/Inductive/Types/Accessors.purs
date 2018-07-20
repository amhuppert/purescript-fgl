module Graph.Inductive.Types.Accessors where

import Graph.Inductive.Types

import Data.List (List)

nodeFromLNode :: forall k a. LNode k a -> k
nodeFromLNode (LNode { node }) = node

labelFromLNode :: forall k a. LNode k a -> a
labelFromLNode (LNode { label }) = label

sourceNodeFromEdge :: forall k. Edge k -> k
sourceNodeFromEdge (Edge { from }) = from

targetNodeFromEdge :: forall k. Edge k -> k
targetNodeFromEdge (Edge { to }) = to

labelFromLEdge :: forall k b. LEdge k b -> b
labelFromLEdge (LEdge { label }) = label

edgeFromLEdge :: forall k b. LEdge k b -> Edge k
edgeFromLEdge (LEdge { edge }) = edge

nodeFromIncidentEdge :: forall k b. IncidentEdge k b -> k
nodeFromIncidentEdge (IncidentEdge { node }) = node

labelFromIncidentEdge :: forall k b. IncidentEdge k b -> b
labelFromIncidentEdge (IncidentEdge { label }) = label

incomersFromContext :: forall k a b. Context k a b -> IncidentEdges k b
incomersFromContext (Context c) = c.incomers

outgoersFromContext :: forall k a b. Context k a b -> IncidentEdges k b
outgoersFromContext (Context c) = c.outgoers

nodeFromContext :: forall k a b. Context k a b -> k
nodeFromContext (Context c) = c.node

labelFromContext :: forall k a b. Context k a b -> a
labelFromContext (Context c) = c.label

edgeLabelsFromEdgeContext :: forall k a b. EdgeContext k a b -> List b
edgeLabelsFromEdgeContext (EdgeContext c) = c.edgeLabels

sourceFromEdgeContext :: forall k a b. EdgeContext k a b -> LNode k a
sourceFromEdgeContext (EdgeContext c) = c.source

targetFromEdgeContext :: forall k a b. EdgeContext k a b -> LNode k a
targetFromEdgeContext (EdgeContext c) = c.target
