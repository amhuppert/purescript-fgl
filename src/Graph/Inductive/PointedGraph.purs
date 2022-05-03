module Graph.Inductive.PointedGraph where

import Prelude hiding (compose)
import Control.Comonad (class Comonad, class Extend)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), uncurry)
import Graph.Inductive.Class (class DynGraph, empty, match, matchAny, merge)
import Graph.Inductive.Types (Context(..), GraphDecomposition(..))

import Data.Lazy (defer, force)
import Effect.Exception (error)
import Effect.Exception.Unsafe (unsafeThrowException)
import Graph.Inductive.Transform (mapNodes)

data PointedGraph gr key edge vert 
  = PointedGraph (Context key vert edge) (gr key vert edge)

mapContext :: forall a b k v. (a -> b) -> Context k a v -> Context k b v 
mapContext f (Context c@{label}) = Context $ c { label = f label}

instance dynGraphPointedGraph :: (Ord key, DynGraph gr) => Functor (PointedGraph gr key edge) where
  map f (PointedGraph context remainder) = PointedGraph (mapContext f context) (mapNodes f remainder)

chooseArbitraryFocus :: forall gr k v e. Ord k => DynGraph gr => gr k v e -> Maybe (PointedGraph gr k e v)
chooseArbitraryFocus gr = case matchAny gr of
  Nothing -> Nothing
  Just (Decomp {context, remaining}) -> Just $ PointedGraph (context) (force remaining)

compose :: forall gr k e v. DynGraph gr => GraphDecomposition gr k v e -> PointedGraph gr k e v
compose (Decomp {context, remaining}) = PointedGraph (context) (force remaining)

decompose :: forall gr k e v. DynGraph gr => PointedGraph gr k e v -> GraphDecomposition gr k v e
decompose (PointedGraph context remaining) = Decomp {context, remaining: defer $ \_ -> remaining}

-- match :: ∀ k a b gr. Graph gr ⇒ Ord k ⇒ k → gr k a b → Maybe (GraphDecomposition gr k a b)

refocus :: forall gr k e v. Ord k => DynGraph gr => PointedGraph gr k e v -> k -> PointedGraph gr k e v
refocus x@(PointedGraph c r) n = case match n r of
  Nothing -> x
  Just _ -> case match n (merge c r) of
    Nothing -> unsafeThrowException (error "Unreachable. If a node is in a subgraph, it is also in the larger graph.")
    (Just d) -> compose d
      

-- -- foldOverDecomp :: DynGraph gr
-- --   => (GDecomp gr vertex edge -> accumulator -> accumulator)
-- --   -> accumulator -> gr vertex edge -> accumulator
-- -- foldOverDecomp _ x Empty = x
-- -- foldOverDecomp f x (Anywhere context remainder) = f (context, remainder) (foldOverDecomp f x remainder)

getNodeFromContext :: forall k v e. Context k v e -> k 
getNodeFromContext (Context {node}) = node

fromDecomp :: forall gr k e v. GraphDecomposition gr k e v -> Tuple (Context k e v) (gr k e v) 
fromDecomp (Decomp {context, remaining}) = Tuple context (force remaining)

-- I think this is the "all rotations" Comonad as described in http://blog.higher-order.com/blog/2016/04/02/a-comonad-of-graph-decompositions/
-- not the "recurive decompositions” comonad.
instance extendGraphGr :: (Ord k, DynGraph gr) => Extend (PointedGraph gr k edge) where
  extend f x@(PointedGraph c _) = case matchAny (extend' x f (uncurry merge (fromDecomp $ decompose x))) of
    Nothing -> unsafeThrowException $ error "Unreachable. There is at least one node in the source and nodes are never dropped."
    Just (Decomp {context, remaining}) -> refocus (compose (Decomp {context, remaining})) (getNodeFromContext c)

extend' :: forall gr k e v v'. Ord k => DynGraph gr => PointedGraph gr k e v -> (PointedGraph gr k e v -> v') -> gr k v e -> gr k v' e
extend' whole f g = case matchAny g of
  Nothing -> empty
  Just (Decomp {context, remaining}) ->
    let
      newLabel = f (refocus whole (getNodeFromContext context))
      newContext = mapContext (const newLabel) context
    in merge newContext (extend' whole f (force remaining))

instance comonadGraphPGr :: (Ord k, DynGraph gr) => Comonad (PointedGraph gr k edge) where
  extract (PointedGraph (Context {label}) _) = label
