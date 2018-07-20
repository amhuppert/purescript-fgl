-- | Depth-first search algorithms.
-- |
-- | Names consist of:
-- |
-- |   1. An optional direction parameter, specifying which nodes to visit next.
-- |
-- |      [@undirected@] undirectional: ignore edge direction
-- |      [@rev@] reversed: walk edges in reverse
-- |      [@x@] user defined: speciy which paths to follow
-- |
-- |   2. "df" for depth-first
-- |   3. A structure parameter, specifying the type of the result.
-- |
-- |       [@s@] Flat list of results
-- |       [@f@] Structured 'Tree' of results
-- |
-- |   4. An optional "All", in which case all nodes of the graph will
-- |      be visited, instead of a user-given subset.
-- |   5. An optional \"With\", which instead of putting the found nodes directly
-- |      into the result, adds the result of a computation on them into it.
module Graph.Inductive.Algorithms.DFS
       ( xdfsWith,
         dfsWith,
         dfs,
         dfsAll,
         dfsAllWith,
         undirectedDfsWith,
         undirectedDfsAllWith,
         undirectedDfs,
         undirectedDfsAll,
         revDfsWith,
         revDfsAllWith,
         revDfs,
         revDfsAll,
         xdffWith,
         xdfWith,
         dff,
         dffWith,
         dffAll,
         dffAllWith,
         undirectedDff,
         undirectedDffAll,
         undirectedDffAllWith,
         revDff,
         revDffWith,
         revDffAll,
         revDffAllWith,
         connectedComponents,
         numConnectedComponents,
         isConnected,
         postflatten,
         postflattenForest,
         topologicalSort,
         topologicalSort',
         stronglyConnectedComponents,
         reachable,
         condensation
       ) where

import Prelude

import Control.Comonad.Cofree as Cofree
import Control.MonadPlus (guard)
import Graph.Inductive.Class (class Graph)
import Graph.Inductive.Class (isEmpty, match, mkGraph) as Graph
import Graph.Inductive.Inspect (hasEdge, nodes) as Graph
import Graph.Inductive.Inspect.Context as Context
import Graph.Inductive.Tree (postorderForest, preorder, preorderForest) as Tree
import Graph.Inductive.Types (Context, Edge(..), GraphDecomposition(..), LEdge(..), LNode(..))
import Graph.Inductive.Types.Accessors as A
import Data.List (List(..))
import Data.List as List
import Data.List.Lazy as LazyList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Tree (Tree)
import Data.Tree (mkTree) as Tree
import Data.Tuple (Tuple(..), fst)
import Data.Tuple as Tuple
import Partial.Unsafe (unsafePartial)

type CFun k a b c = Context k a b -> c

-- | Most general DFS algorithm to create a list of results. The other
-- | list-returning functions such as 'dfs' are all defined in terms of this one.
xdfsWith :: forall gr k a b c. Graph gr => Ord k
         => CFun k a b (List k) -- ^ controls which neighboring nodes to visit next
         -> CFun k a b c -- ^ Mapping from a context of a node to a result value.
         -> List k  -- ^ Nodes to be visited
         -> gr k a b
         -> List c
xdfsWith _ _ Nil _ = Nil
xdfsWith _ _ _ g | Graph.isEmpty g = Nil
xdfsWith getVisitNext f (Cons v vs) g =
  case Graph.match v g of
    Nothing -> go vs g
    Just (Decomp {context, remaining})-> f context `Cons` go (getVisitNext context <> vs) remaining

  where go = xdfsWith getVisitNext f

dfsWith  :: forall gr k a b c. Ord k => Graph gr
         => CFun k a b c
         -> List k
         -> gr k a b
         -> List c
dfsWith = xdfsWith Context.successors

dfsAllWith  :: forall gr k a b c. Ord k => Graph gr
         => CFun k a b c
         -> gr k a b
         -> List c
dfsAllWith f = withAllNodes (dfsWith f)

dfs :: forall gr k a b. Ord k => Graph gr
        => List k
        -> gr k a b
        -> List k
dfs = dfsWith A.nodeFromContext

dfsAll :: forall gr k a b. Ord k => Graph gr => gr k a b -> List k
dfsAll = withAllNodes dfs


withAllNodes :: forall a b c gr k. Graph gr =>
                (List k -> gr k a b -> c)
             -> gr k a b
             -> c
withAllNodes f g = f (Graph.nodes g) g

undirectedDfsWith  :: forall gr k a b c. Graph gr => Ord k
         => CFun k a b c
         -> List k
         -> gr k a b
         -> List c
undirectedDfsWith = xdfsWith Context.neighbors

undirectedDfsAllWith  :: forall gr k a b c. Graph gr => Ord k
         => CFun k a b c
         -> gr k a b
         -> List c
undirectedDfsAllWith f = withAllNodes (undirectedDfsWith f)

undirectedDfs :: forall gr k a b. Graph gr => Ord k
        => List k
        -> gr k a b
        -> List k
undirectedDfs = undirectedDfsWith A.nodeFromContext

undirectedDfsAll :: forall gr k a b. Graph gr => Ord k
        => gr k a b
        -> List k
undirectedDfsAll = withAllNodes undirectedDfs

revDfsWith  :: forall gr k a b c. Graph gr => Ord k
         => CFun k a b c
         -> List k
         -> gr k a b
         -> List c
revDfsWith = xdfsWith Context.predecessors

revDfsAllWith  :: forall gr k a b c. Graph gr => Ord k
         => CFun k a b c
         -> gr k a b
         -> List c
revDfsAllWith f = withAllNodes (undirectedDfsWith f)

revDfs :: forall gr k a b. Graph gr => Ord k
        => List k
        -> gr k a b
        -> List k
revDfs = undirectedDfsWith A.nodeFromContext

revDfsAll :: forall gr k a b. Graph gr => Ord k
        => gr k a b
        -> List k
revDfsAll = withAllNodes undirectedDfs

-- | Most general DFS algorithm to create a forest of results, otherwise very
-- | similar to 'xdfsWith'. The other forest-returning functions such as 'dff'
-- | are all defined in terms of this one.
xdfWith :: forall gr k a b c. Graph gr => Ord k
    => CFun k a b (List k)
    -> CFun k a b c
    -> List k
    -> gr k a b
    -> Tuple (List (Tree c)) (gr k a b)
xdfWith _ _ Nil g = Tuple Nil g
xdfWith _ _ _ g | Graph.isEmpty g = Tuple Nil g
xdfWith d f (Cons v vs) g = case Graph.match v g of
  Nothing -> xdfWith d f vs g
  Just (Decomp { context, remaining })->
    let Tuple ts g2 = xdfWith d f (d context) remaining
        Tuple ts' g3 = xdfWith d f vs g2
     in Tuple (Tree.mkTree (f context) ts `Cons` ts') g3

-- | Discard the graph part of the result of 'xdfWith'
xdffWith :: forall gr k a b c. Graph gr => Ord k
    => CFun k a b (List k)
    -> CFun k a b c
    -> List k
    -> gr k a b
    -> List (Tree c)
xdffWith d f vs g = fst (xdfWith d f vs g)

-- | Directed depth-first forest.
dff :: forall gr k a b. Ord k => Graph gr => List k -> gr k a b -> List (Tree k)
dff = dffWith A.nodeFromContext

dffWith :: forall gr k a b c. Ord k => Graph gr => CFun k a b c -> List k -> gr k a b -> List (Tree c)
dffWith = xdffWith Context.successors

dffAllWith :: forall gr k a b c. Ord k => Graph gr => CFun k a b c -> gr k a b -> List (Tree c)
dffAllWith f = withAllNodes (dffWith f)

dffAll :: forall gr k a b. Ord k => Graph gr => gr k a b -> List (Tree k)
dffAll = dffAllWith A.nodeFromContext

-- | Undirected depth-first forest.
undirectedDff :: forall gr k a b. Ord k => Graph gr => List k -> gr k a b -> List (Tree k)
undirectedDff = undirectedDffWith A.nodeFromContext

undirectedDffWith :: forall gr k a b c. Ord k => Graph gr => CFun k a b c -> List k -> gr k a b -> List (Tree c)
undirectedDffWith = xdffWith Context.neighbors

undirectedDffAllWith :: forall gr k a b c. Ord k => Graph gr => CFun k a b c -> gr k a b -> List (Tree c)
undirectedDffAllWith f = withAllNodes (undirectedDffWith f)

undirectedDffAll :: forall gr k a b. Ord k => Graph gr => gr k a b -> List (Tree k)
undirectedDffAll = undirectedDffAllWith A.nodeFromContext

-- | Reverse depth-first forest.
revDff :: forall gr k a b. Ord k => Graph gr => List k -> gr k a b -> List (Tree k)
revDff = revDffWith A.nodeFromContext

revDffWith :: forall gr k a b c. Ord k => Graph gr => CFun k a b c -> List k -> gr k a b -> List (Tree c)
revDffWith = xdffWith Context.predecessors

revDffAllWith :: forall gr k a b c. Ord k => Graph gr => CFun k a b c -> gr k a b -> List (Tree c)
revDffAllWith f = withAllNodes (revDffWith f)

revDffAll :: forall gr k a b. Ord k => Graph gr => gr k a b -> List (Tree k)
revDffAll = revDffAllWith A.nodeFromContext

----------------------------------------------------------------------
-- ALGORITHMS BASED ON DFS
----------------------------------------------------------------------

-- | Collection of connected components
connectedComponents :: forall gr k a b. Ord k => Graph gr => gr k a b -> List (List k)
connectedComponents = map Tree.preorder <<< undirectedDffAll

-- | Number of connected components
numConnectedComponents :: forall gr k a b. Ord k => Graph gr => gr k a b -> Int
numConnectedComponents = List.length <<< connectedComponents

-- | Is the graph connected?
isConnected :: forall gr k a b. Ord k => Graph gr => gr k a b -> Boolean
isConnected = (_ == 1) <<< numConnectedComponents

-- | Flatten a 'Tree' in reverse order
postflatten :: forall a. Tree a -> List a
postflatten t = postflattenForest (Cofree.tail t) <> List.singleton (Cofree.head t)

-- | Flatten a forest in reverse order
postflattenForest :: forall a. List (Tree a) -> List a
postflattenForest ts = List.concatMap postflatten ts

-- | <http://en.wikipedia.org/wiki/Topological_sorting Topological sorting>,
-- | i.e. a list of 'Node's so that if there's an edge between a source and a
-- | target node, the source appears earlier in the result.
topologicalSort :: forall gr k a b. Ord k => Graph gr => gr k a b -> List k
topologicalSort = List.reverse <<< postflattenForest <<< dffAll

-- | 'topologicalSort', returning only the labels of the nodes.
topologicalSort' :: forall gr k a b. Ord k => Graph gr => gr k a b -> List a
topologicalSort' = List.reverse <<< Tree.postorderForest <<< dffAllWith A.labelFromContext

-- | Collection of strongly connected components
stronglyConnectedComponents :: forall gr k a b. Ord k => Graph gr => gr k a b -> List (List k)
stronglyConnectedComponents g = map Tree.preorder (revDff (topologicalSort g) g)

-- | Collection of nodes reachable from a starting point.
reachable :: forall gr k a b. Ord k => Graph gr => k -> gr k a b -> List k
reachable v g = Tree.preorderForest (dff (List.singleton v) g)

-- | The condensation of the given graph, i.e., the graph of its
-- | strongly connected components.
condensation :: forall gr k a b. Ord k => Ord k => Graph gr => gr k a b -> gr Int (List k) Unit
condensation gr = Graph.mkGraph vs' es
  where
    sccs :: List (List k)
    sccs = stronglyConnectedComponents gr

    vs :: List (Tuple Int (List k))
    vs = List.fromFoldable $
           LazyList.zip (LazyList.iterate (_ + 1) 1) (LazyList.fromFoldable sccs)

    vs' = map (\(Tuple node label) -> LNode { node, label }) vs

    vMap :: Map (List k) Int
    vMap = Map.fromFoldable $ map Tuple.swap vs

    getN :: List k -> Int
    getN = (vMap `unsafeLookup` _)

    es = do
      c1 <- sccs
      c2 <- sccs
      guard (c1 /= c2 && anyHasEdge c1 c2)
      pure $ LEdge { edge: Edge { from: getN c1
                                , to: getN c2
                                }
                   , label: unit
                   }

    anyHasEdge c1 c2 = List.any (Graph.hasEdge gr) do
      x <- c1
      y <- c2
      pure $ Edge { from: x, to: y }

    unsafeLookup m k = unsafePartial (fromJust (Map.lookup k m))