module Data.Graph.Inductive.Construction where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldM, foldr)
import Data.Graph.Inductive.Core (class DynGraph, class Graph, Adj, Context, Edge, LEdge, LNode, Node, UGraph, (&))
import Data.Graph.Inductive.Core as Core
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..))

insNode :: forall gr a b. DynGraph gr => LNode a -> gr a b -> gr a b
insNode (Tuple node label) gr = Core.unsafeMerge newContext gr
  where newContext = { incomers: [], node, label, outgoers: [] }

insNodes :: forall gr a b. DynGraph gr => Array (LNode a) -> gr a b -> gr a b
insNodes ns g = foldr insNode g ns

insEdge :: forall gr a b. DynGraph gr => LEdge b -> gr a b -> Either String (gr a b)
insEdge (Tuple edge label) gr =
  maybe
    (Left $ "insEdge: Cannot insert edge from non-existent vertex "
          <> show edge.from)
    (\c -> add c.context & c.remaining)
    source

  where source = Core.match edge.from gr
        add c = c { outgoers = c.outgoers `Array.snoc` (Tuple label edge.to) }

delNodes :: forall gr a b. DynGraph gr => Array Node -> gr a b -> gr a b
delNodes vs g = foldr rm g vs
  where rm v currG = case Core.match v currG of
          Nothing -> currG
          Just { context, remaining } -> remaining

delNode :: forall gr a b. DynGraph gr => Node -> gr a b -> gr a b
delNode v = delNodes [v]

delEdge :: forall gr a b. DynGraph gr => Edge -> gr a b -> gr a b
delEdge e graph =
  case Core.match e.from graph of
    Nothing -> graph
    Just { context: c, remaining } ->
      let c' = c { outgoers = Array.filter nEq c.outgoers }
       in c' `Core.unsafeMerge` remaining
  where nEq (Tuple _ o) = o /= e.to

delEdges :: forall gr a b. DynGraph gr => Array Edge -> gr a b -> gr a b
delEdges es g = foldr delEdge g es

delLEdgeBy :: forall gr a b. DynGraph gr =>
              (Tuple b Node -> Adj b -> Adj b)
           -> LEdge b
           -> gr a b
           -> gr a b
delLEdgeBy f (Tuple e label) g =
  case Core.match e.from g of
    Nothing -> g
    Just { context: c, remaining } ->
      c { outgoers = f (Tuple label e.to) c.outgoers } `Core.unsafeMerge` remaining

-- | Delete all edges equal to the one specified
delLEdges :: forall gr a b. DynGraph gr => Eq b => LEdge b -> gr a b -> gr a b
delLEdges  = delLEdgeBy (Array.filter <<< ne)
  where ne (Tuple b1 n1) (Tuple b2 n2) = not (n1 == n2 && b1 == b2)

-- | Build a Graph from a list of Contexts.
--   The list should be in the order such that earlier Contexts depend upon later ones
buildGraph :: forall gr a b f. DynGraph gr => Foldable f
            => f (Context a b)
            -> Either String (gr a b)
buildGraph = foldM (flip Core.merge) Core.empty

mkUnlabeledGraph :: forall t gr. Traversable t => Graph gr
                    => t Node
                    -> t Edge
                    -> UGraph gr
mkUnlabeledGraph nodes edges =
  let le = map (_ `Core.labelEdge` unit) edges
      ln = map (_ `Core.labelNode` unit) nodes
   in Core.mkGraph ln le
