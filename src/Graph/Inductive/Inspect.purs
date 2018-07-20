module Graph.Inductive.Inspect where

import Graph.Inductive.Class
import Graph.Inductive.Types
import Prelude

import Graph.Inductive.Core (fold)
import Graph.Inductive.Inspect.Context as Context
import Graph.Inductive.Types.Accessors as A
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe, fromMaybe, isJust)
import Data.Newtype (unwrap)

context :: forall gr k a b. Ord k => Graph gr => gr k a b -> k -> Maybe (Context k a b)
context g n = (unwrap >>> _.context) <$> match n g

contexts :: forall gr k a b. Ord k => Graph gr => gr k a b -> List (Context k a b)
contexts g = List.mapMaybe (context g) (nodes g)

nodes :: forall gr k a b. Graph gr => gr k a b -> List k
nodes = labNodes >>> map A.nodeFromLNode

edges :: forall gr k a b. Graph gr => gr k a b -> List (Edge k)
edges = labEdges >>> map A.edgeFromLEdge

neighbors :: forall gr k a b. Ord k => Graph gr => gr k a b -> k -> (List k)
neighbors g n =
  let ns = Context.neighbors <$> context g n
   in fromMaybe Nil ns

successors :: forall gr k a b. Ord k => Graph gr => gr k a b -> k -> (List k)
successors g v =
  let succs = Context.successors <$> context g v
   in fromMaybe Nil succs

predecessors :: forall gr k a b. Ord k => Graph gr => gr k a b -> k -> (List k)
predecessors g v =
  let succs = Context.predecessors <$> context g v
   in fromMaybe Nil succs

outgoers :: forall gr k a b. Ord k => Graph gr => gr k a b -> k -> (List (LEdge k b))
outgoers g v =
  let outs = Context.outgoers <$> context g v
   in fromMaybe Nil outs

incomers :: forall gr k a b. Ord k => Graph gr => gr k a b -> k -> (List (LEdge k b))
incomers g v =
  let ins = Context.incomers <$> context g v
   in fromMaybe Nil ins

outDegree :: forall gr k a b. Ord k => Graph gr => gr k a b -> k -> Int
outDegree g v =
  let d = Context.outDegree <$> context g v
   in fromMaybe 0 d

inDegree :: forall gr k a b. Ord k => Graph gr => gr k a b -> k -> Int
inDegree g v =
  let d = Context.inDegree <$> context g v
   in fromMaybe 0 d

-- | The degree of the 'Node'.
degree :: forall gr k a b. Ord k => Graph gr => gr k a b -> k -> Int
degree g v =
  let d = Context.degree <$> context g v
   in fromMaybe 0 d

-- | Return all 'Context's for which the given function returns 'True'.
selectContexts :: forall gr k a b. Ord k => Graph gr => (Context k a b -> Boolean) -> gr k a b -> List (Context k a b)
selectContexts p = fold (\c cs -> if p c then c `Cons` cs else cs) Nil


-------------------------------------------------------------------------------------
-- Predicates -----------------------------------------------------------------------
-------------------------------------------------------------------------------------

-- | True if the graph has any edges of the form (A, A).
hasLoop :: forall gr k a b. Ord k => Graph gr => gr k a b -> Boolean
hasLoop = not <<< List.null <<< selectContexts checkSuc
  where checkSuc (Context c) = c.node `List.elem` Context.successors (Context c)

-- | True if the Node is present in the Graph
elem :: forall gr k a b. Ord k => Graph gr =>k -> gr k a b -> Boolean
elem n = match n >>> isJust

-- | Checks if there is a directed edge between two nodes.
hasEdge :: forall gr k a b. Ord k => Graph gr => gr k a b -> Edge k -> Boolean
hasEdge gr (Edge e) = e.to `List.elem` successors gr e.from

-- | Checks if there is an edge between two nodes.
hasNeighbor :: forall gr k a b. Ord k => Graph gr => gr k a b -> k -> k -> Boolean
hasNeighbor gr v w = List.elem w $ neighbors gr v

nodeLabel :: forall gr k a b. Ord k => Graph gr => gr k a b -> k -> Maybe a
nodeLabel g n = A.labelFromContext <$> context g n
