module Graph.Inductive.Core  where

import Prelude
import Graph.Inductive.Class (class DynGraph, class Graph, class OrdGraph, isEmpty, labEdges, matchAny, maxNode, mkGraph, (&))
import Graph.Inductive.Types (Context(..), Edge(..), GraphDecomposition(..), IncidentEdge(..), LEdge(..), LNode(..), UGraph)
import Data.Bounded as Bounded
import Data.Enum (class Enum)
import Data.Enum as Enum
import Data.Foldable (class Foldable, foldl, foldr)
import Graph.Inductive.Class as Graph
import Data.Lazy as Lazy
import Data.List (List(..))
import Data.List as List
import Data.List.Lazy as LL
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Traversable (class Traversable)
import Partial.Unsafe (unsafePartial)

labelEdge :: forall k b. Edge k -> b -> LEdge k b
labelEdge e lab = LEdge { edge: e , label: lab }

labelNode :: forall k a. k -> a -> LNode k a
labelNode n lab = LNode { node: n, label: lab }

newNodes :: forall gr k a b. Enum k => Bounded k => OrdGraph gr => Int -> gr k a b -> LL.List k
newNodes count graph =
  case maxNode graph of
    Just max -> LL.List (Lazy.defer (\_ -> newNodes' count (unsafeSucc max)))
    Nothing -> LL.List (Lazy.defer (\_ -> newNodes' count Bounded.bottom))
  where newNodes' :: Int -> k -> LL.Step k
        newNodes' remaining _ | remaining <= 0 = LL.Nil
        newNodes' remaining from =
          let rest = LL.List (Lazy.defer (\_ -> newNodes' (remaining-1) (unsafeSucc from)))
           in from `LL.Cons` rest
        unsafeSucc v = unsafePartial $ fromJust $ Enum.succ v

newNode :: forall gr k a b. Enum k => Bounded k => OrdGraph gr => gr k a b -> k
newNode graph = unsafePartial $ fromJust $ LL.head $ newNodes 1 graph

numEdges :: forall gr k a b. (Graph gr) => gr k a b -> Int
numEdges = labEdges >>> List.length

-- | Fold a function over the Graph by recursively calling matchAny
fold :: forall gr k a b c. Ord k => Graph gr => (Context k a b -> c -> c) -> c -> gr k a b -> c
fold f accum graph
  | isEmpty graph = accum
  | otherwise =
      let Decomp { context, remaining } = unsafePartial (fromJust (matchAny graph))
       in f context (fold f accum remaining)

mkUnlabeledGraph :: forall t gr k. Traversable t => Graph gr => Ord k
                    => t k
                    -> t (Edge k)
                    -> UGraph gr k
mkUnlabeledGraph nodes edges =
  let le = map (_ `labelEdge` unit) edges
      ln = map (_ `labelNode` unit) nodes
   in mkGraph ln le

insNode :: forall gr k a b. Ord k => DynGraph gr => LNode k a -> gr k a b -> gr k a b
insNode (LNode { node, label }) gr = Graph.merge newContext gr
  where newContext = Context { incomers: Nil, node, label, outgoers: Nil }

insNodes :: forall gr k a b. Ord k => DynGraph gr => List (LNode k a) -> gr k a b -> gr k a b
insNodes ns g = foldr insNode g ns

insEdge :: forall gr k a b. Ord k => DynGraph gr => LEdge k b -> gr k a b -> gr k a b
insEdge (LEdge { edge: Edge edge, label }) gr =
  maybe
    gr
    (\(Decomp { context, remaining }) -> add context & remaining)
    (Graph.match edge.from gr)

  where add (Context c) =
          let newOut = IncidentEdge { node: edge.to, label }
              contextRec = c { outgoers = newOut `Cons` c.outgoers }
           in Context contextRec

insEdges :: forall gr k a b f. Ord k => DynGraph gr => Foldable f => f (LEdge k b) -> gr k a b -> gr k a b
insEdges es g = foldl (flip insEdge) g es

delNodes :: forall gr k a b. Ord k => DynGraph gr => List k -> gr k a b -> gr k a b
delNodes vs g = foldr rm g vs
  where rm v currG = case Graph.match v currG of
          Nothing -> currG
          Just (Decomp { context, remaining }) -> remaining

delNode :: forall gr k a b. Ord k => DynGraph gr =>k -> gr k a b -> gr k a b
delNode v = delNodes $ List.singleton v

delEdge :: forall gr k a b. Ord k => DynGraph gr => Edge k -> gr k a b -> gr k a b
delEdge (Edge e) graph =
  case Graph.match e.from graph of
    Nothing -> graph
    Just (Decomp { context: Context c, remaining }) ->
      let c' = c { outgoers = List.filter nEq c.outgoers }
       in Context c' `Graph.merge` remaining
  where nEq (IncidentEdge { node }) = node /= e.to

delEdges :: forall gr k a b f. Ord k => DynGraph gr => Foldable f => f (Edge k) -> gr k a b -> gr k a b
delEdges es g = foldr delEdge g es
