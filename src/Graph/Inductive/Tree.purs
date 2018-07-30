module Graph.Inductive.Tree
       ( postorder
       , postorderForest
       , preorder
       , preorderForest
       , Tree(..)
       , Forest(..)
       , mkTree
       , subForest
       , root
       )
       where


import Prelude

import Data.Foldable (class Foldable)
import Data.List.Lazy (List)
import Data.List.Lazy as List

data Tree a = Tree a (Forest a)

type Forest a = List (Tree a)

mkTree :: forall a f. Foldable f => a -> f (Tree a) -> Tree a
mkTree v forest = Tree v (List.fromFoldable forest)

subForest :: forall a. Tree a -> Forest a
subForest (Tree _ forest) = forest

root :: forall a. Tree a -> a
root (Tree v _) = v

-- | Flatten a 'Tree', returning the elements in post-order.
postorder :: forall a. Tree a -> List a
postorder t =
  postorderForest (subForest t) <> List.singleton (root t)

-- | Flatten multiple 'Tree's in post-order.
postorderForest :: forall a. List (Tree a) -> List a
postorderForest ts = List.concatMap postorder ts

-- | Flatten a 'Tree', returning the elements in pre-order.  Equivalent to
--'flatten' in 'Data.Tree'.
preorder :: forall a. Tree a -> List a
preorder t = squish t List.nil
  where squish (Tree r forest) xs =
          r `List.cons` List.foldrLazy squish xs forest

-- | Flatten multiple 'Tree's in pre-order.
preorderForest :: forall a. List (Tree a) -> List a
preorderForest = List.concatMap preorder
