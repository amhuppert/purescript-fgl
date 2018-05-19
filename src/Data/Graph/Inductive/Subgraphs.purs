module Data.Graph.Inductive.Subgraphs
  ( filterMap
  , filterNodes
  , filterLNodes
  , filterNodesOnLabel
  , filterEdges
  , filterEdgesOnLabel
  , subgraph
  ) where

import Prelude

import Data.Array as Array
import Data.Either as Either
import Data.Foldable (class Foldable)
import Data.Graph.Inductive.Construction (delNodes)
import Data.Graph.Inductive.Core (class DynGraph, Context, LNode, Node, LEdge, (&))
import Data.Graph.Inductive.Core as Core
import Data.Maybe (Maybe, fromMaybe)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)

filterMap :: forall gr a b c d. DynGraph gr
          => (Context a b -> Maybe (Context c d))
          -> gr a b
          -> gr c d
filterMap f = Core.fold foldFunc Core.empty
  where foldFunc c graph = fromMaybe graph do
          c' <- f c
          Either.hush (c' & graph)

filterNodes :: forall gr a b. DynGraph gr
            => (Node -> Boolean)
            -> gr a b
            -> gr a b
filterNodes p = filterLNodes (p <<< fst)

filterNodesOnLabel :: forall gr a b. DynGraph gr
            => (a -> Boolean)
            -> gr a b
            -> gr a b
filterNodesOnLabel p = filterLNodes (p <<< snd)

filterLNodes :: forall gr a b. DynGraph gr
             => (LNode a -> Boolean)
             -> gr a b
             -> gr a b
filterLNodes p gr =
  delNodes (map fst <<< Array.filter (not <<< p) $ Core.labNodes gr) gr

-- | Returns the subgraph induced by the supplied nodes
subgraph :: forall gr a b f. DynGraph gr => Foldable f
         => f Node
         -> gr a b
         -> gr a b
subgraph vs = let vs'= Set.fromFoldable vs
               in filterNodes (_ `Set.member` vs')

-- | Filter based on edge property.
filterEdges :: forall gr a b. DynGraph gr => (LEdge b -> Boolean) -> gr a b -> gr a b
filterEdges f = Core.fold cfilter Core.empty
  where cfilter c g = c { incomers = incomers
                        , outgoers = outgoers
                        }
                      `Core.unsafeMerge` g
          where incomers = Array.filter (\(Tuple b u) -> f $ Tuple { from: u, to: c.node } b) c.incomers
                outgoers = Array.filter (\(Tuple b w) -> f $ Tuple { from: c.node, to: w } b) c.outgoers

-- | Filter based on edge label property.
filterEdgesOnLabel :: forall gr a b. DynGraph gr => (b -> Boolean) -> gr a b -> gr a b
filterEdgesOnLabel f = filterEdges (\(Tuple _ b) -> f b)
