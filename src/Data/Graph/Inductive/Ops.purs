module Data.Graph.Inductive.Ops where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Graph.Inductive.Core (class DynGraph, Edge, LEdge, LNode, Node, Adj, (&))
import Data.Graph.Inductive.Core as Core
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))

insNode :: forall gr a b. DynGraph gr => LNode a -> gr a b -> gr a b
insNode (Tuple node label) gr = Core.unsafeMerge newContext gr
  where newContext = { incomers: [], node, label, outgoers: [] }

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
delLEdge :: forall gr a b. DynGraph gr => Eq b => LEdge b -> gr a b -> gr a b
delLEdge  = delLEdgeBy (Array.filter <<< ne)
  where ne (Tuple b1 n1) (Tuple b2 n2) = not (n1 == n2 && b1 == b2)

---------------------------
-- UTILITIES --------------
---------------------------

compose2 :: forall a b c d. (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 = compose <<< compose

infixr 8 compose2 as .:

