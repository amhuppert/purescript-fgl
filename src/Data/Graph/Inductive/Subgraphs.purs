module Data.Graph.Inductive.Subgraphs
  ( filterMap
  , filterOnNodes
  , filterOnLNodes
  , filterOnNodeLabels
  , subgraph
  ) where

import Data.Graph.Inductive.Construction (delNodes)
import Prelude

import Data.Array as Array
import Data.Either as Either
import Data.Foldable (class Foldable)
import Data.Graph.Inductive.Core (class DynGraph, Context, Node, LNode, (&))
import Data.Graph.Inductive.Core as Core
import Data.Maybe (Maybe, fromMaybe)
import Data.Set as Set
import Data.Tuple (fst, snd)

filterMap :: forall gr a b c d. DynGraph gr
          => (Context a b -> Maybe (Context c d))
          -> gr a b
          -> gr c d
filterMap f = Core.fold foldFunc Core.empty
  where foldFunc c graph = fromMaybe graph do
          c' <- f c
          Either.hush (c' & graph)

filterOnNodes :: forall gr a b. DynGraph gr
            => (Node -> Boolean)
            -> gr a b
            -> gr a b
filterOnNodes p = filterOnLNodes (p <<< fst)

filterOnNodeLabels :: forall gr a b. DynGraph gr
            => (a -> Boolean)
            -> gr a b
            -> gr a b
filterOnNodeLabels p = filterOnLNodes (p <<< snd)

filterOnLNodes :: forall gr a b. DynGraph gr
             => (LNode a -> Boolean)
             -> gr a b
             -> gr a b
filterOnLNodes p gr =
  delNodes (map fst <<< Array.filter (not <<< p) $ Core.labNodes gr) gr

-- | Returns the subgraph induced by the supplied nodes
subgraph :: forall gr a b f. DynGraph gr => Foldable f
         => f Node
         -> gr a b
         -> gr a b
subgraph vs = let vs'= Set.fromFoldable vs
               in filterOnNodes (_ `Set.member` vs')
