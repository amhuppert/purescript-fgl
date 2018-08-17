module Graph.Inductive.Algorithms.BFS
       ( bfs
       , bfsWith
       , bfsMultiSource
       , bfsMultiSourceWith
       ) where

import Prelude

import Data.CatQueue (CatQueue)
import Data.CatQueue as Q
import Data.Foldable (class Foldable)
import Data.Lazy as Lazy
import Data.List.Lazy (List(..))
import Data.List.Lazy as List
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Graph.Inductive.Class (class Graph)
import Graph.Inductive.Class as Graph
import Graph.Inductive.Inspect.Context as Context
import Graph.Inductive.Types (Context, GraphDecomposition(..))

-- | Get a list of nodes in breadth-first order, starting with the provided source node.
bfs :: forall gr k a b. Graph gr => Ord k => k -> gr k a b -> List k
bfs node graph = bfsWith node Context.node graph

-- | Like `bfs` but projecting from context to result type.
bfsWith :: forall gr k a b c. Graph gr => Ord k => k -> (Context k a b -> c) -> gr k a b -> List c
bfsWith node contextToResult g = bfsInternal contextToResult (Q.singleton node) g

bfsMultiSource :: forall gr k a b f. Graph gr => Ord k => Foldable f => f k ->  gr k a b -> List k
bfsMultiSource sourceNodes graph = bfsMultiSourceWith sourceNodes Context.node graph

bfsMultiSourceWith :: forall gr k a b c f. Graph gr => Ord k => Foldable f => f k -> (Context k a b -> c) -> gr k a b -> List c
bfsMultiSourceWith sourceNodes contextToResult g = bfsInternal contextToResult (Q.fromFoldable sourceNodes) g

bfsInternal :: forall gr k a b c. Ord k => Graph gr => (Context k a b -> c) -> CatQueue k -> gr k a b -> List c
bfsInternal contextToResult queue graph = maybe List.nil bfsWithQueueDecomp (Q.uncons queue)
  where
    bfsInternal' = bfsInternal contextToResult

    bfsWithQueueDecomp (Tuple next queue') =
      case Graph.match next graph of
        Just (Decomp { context, remaining }) ->
          let result = contextToResult context
              successors = Q.fromFoldable $ Context.successors context
              lazyRest = List $
                         Lazy.defer \_ ->
                         List.Cons result (bfsInternal' (queue' <> successors) (Lazy.force remaining))
           in lazyRest
        Nothing -> bfsInternal' queue' graph
