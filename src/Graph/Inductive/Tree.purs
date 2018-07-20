module Graph.Inductive.Tree
       ( postorder
       , postorderForest
       , preorder
       , preorderForest
       ) where


-- | Flatten a 'Tree', returning the elements in post-order.
import Control.Comonad.Cofree as Cofree
import Data.Foldable (foldr)
import Data.List (List(..))
import Data.List as List
import Data.Monoid ((<>))
import Data.Tree (Tree)


-- | Flatten a 'Tree', returning the elements in post-order.
postorder :: forall a. Tree a -> List a
postorder t =
  postorderForest (Cofree.tail t) <> List.singleton (Cofree.head t)

-- | Flatten multiple 'Tree's in post-order.
postorderForest :: forall a. List (Tree a) -> List a
postorderForest ts = List.concatMap postorder ts

-- | Flatten a 'Tree', returning the elements in pre-order.  Equivalent to
--'flatten' in 'Data.Tree'.
preorder :: forall a. Tree a -> List a
preorder t = squish t Nil
  where squish t' xs =
          Cofree.head t' `Cons` foldr squish xs (Cofree.tail t')

-- | Flatten multiple 'Tree's in pre-order.
preorderForest :: forall a. List (Tree a) -> List a
preorderForest = List.concatMap preorder
