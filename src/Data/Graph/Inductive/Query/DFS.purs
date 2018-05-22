-- | Depth-first search algorithms.
--
-- Names consist of:
--
--   1. An optional direction parameter, specifying which nodes to visit next.
--
--      [@undirected@] undirectional: ignore edge direction
--      [@rev@] reversed: walk edges in reverse
--      [@x@] user defined: speciy which paths to follow
--
--   2. "df" for depth-first
--   3. A structure parameter, specifying the type of the result.
--
--       [@s@] Flat list of results
--       [@f@] Structured 'Tree' of results
--
--   4. An optional "All", in which case all nodes of the graph will
--      be visited, instead of a user-given subset.
--   5. An optional \"With\", which instead of putting the found nodes directly
--      into the result, adds the result of a computation on them into it.
module Data.Graph.Inductive.Query.DFS
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
import Data.Either (fromRight)
import Data.Graph.Inductive.Core (class Graph, Context, Node)
import Data.Graph.Inductive.Core as Graph
import Data.List (List(..))
import Data.List as List
import Data.List.Lazy as LazyList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Tree (Tree)
import Data.Tree as Tree
import Data.Tuple (Tuple(..), fst)
import Data.Tuple as Tuple
import Partial.Unsafe (unsafePartial)

type CFun a b c = Context a b -> c

-- | Most general DFS algorithm to create a list of results. The other
--   list-returning functions such as 'dfs' are all defined in terms of this
--   one.
--
xdfsWith :: forall gr a b c. Graph gr
         => CFun a b (List Node) -- ^ controls which neighboring nodes to visit next
         -> CFun a b c -- ^ Mapping from a context of a node to a result value.
         -> List Node  -- ^ Nodes to be visited
         -> gr a b
         -> List c
xdfsWith _ _ Nil _ = Nil
xdfsWith _ _ _ g | Graph.isEmpty g = Nil
xdfsWith getVisitNext f (Cons v vs) g =
  case Graph.match v g of
    Nothing -> go vs g
    Just {context, remaining} -> f context `Cons` go (getVisitNext context <> vs) remaining

  where go = xdfsWith getVisitNext f

dfsWith  :: forall gr a b c. Graph gr
         => CFun a b c
         -> List Node
         -> gr a b
         -> List c
dfsWith = xdfsWith (Graph.suc' >>> List.fromFoldable)

dfsAllWith  :: forall gr a b c. Graph gr
         => CFun a b c
         -> gr a b
         -> List c
dfsAllWith f = withAllNodes (dfsWith f)

dfs :: forall gr a b. Graph gr
        => List Node
        -> gr a b
        -> List Node
dfs = dfsWith _.node

dfsAll :: forall gr a b. Graph gr => gr a b -> List Node
dfsAll = withAllNodes dfs


withAllNodes :: forall a b c gr. Graph gr =>
                (List Node -> gr a b -> c)
             -> gr a b
             -> c
withAllNodes f g = f (List.fromFoldable $ Graph.nodes g) g

undirectedDfsWith  :: forall gr a b c. Graph gr
         => CFun a b c
         -> List Node
         -> gr a b
         -> List c
undirectedDfsWith = xdfsWith (Graph.neighbors' >>> List.fromFoldable)

undirectedDfsAllWith  :: forall gr a b c. Graph gr
         => CFun a b c
         -> gr a b
         -> List c
undirectedDfsAllWith f = withAllNodes (undirectedDfsWith f)

undirectedDfs :: forall gr a b. Graph gr
        => List Node
        -> gr a b
        -> List Node
undirectedDfs = undirectedDfsWith _.node

undirectedDfsAll :: forall gr a b. Graph gr
        => gr a b
        -> List Node
undirectedDfsAll = withAllNodes undirectedDfs

revDfsWith  :: forall gr a b c. Graph gr
         => CFun a b c
         -> List Node
         -> gr a b
         -> List c
revDfsWith = xdfsWith (Graph.pre' >>> List.fromFoldable)

revDfsAllWith  :: forall gr a b c. Graph gr
         => CFun a b c
         -> gr a b
         -> List c
revDfsAllWith f = withAllNodes (undirectedDfsWith f)

revDfs :: forall gr a b. Graph gr
        => List Node
        -> gr a b
        -> List Node
revDfs = undirectedDfsWith _.node

revDfsAll :: forall gr a b. Graph gr
        => gr a b
        -> List Node
revDfsAll = withAllNodes undirectedDfs

-- | Most general DFS algorithm to create a forest of results, otherwise very
--   similar to 'xdfsWith'. The other forest-returning functions such as 'dff'
--   are all defined in terms of this one.
xdfWith :: forall gr a b c. Graph gr
    => CFun a b (List Node)
    -> CFun a b c
    -> List Node
    -> gr a b
    -> Tuple (List (Tree c)) (gr a b)
xdfWith _ _ Nil g = Tuple Nil g
xdfWith _ _ _ g | Graph.isEmpty g = Tuple Nil g
xdfWith d f (Cons v vs) g = case Graph.match v g of
  Nothing -> xdfWith d f vs g
  Just { context, remaining } ->
    let Tuple ts g2 = xdfWith d f (d context) remaining
        Tuple ts' g3 = xdfWith d f vs g2
     in Tuple (Tree.mkTree (f context) ts `Cons` ts') g3

-- | Discard the graph part of the result of 'xdfWith'
xdffWith :: forall gr a b c. Graph gr
    => CFun a b (List Node)
    -> CFun a b c
    -> List Node
    -> gr a b
    -> List (Tree c)
xdffWith d f vs g = fst (xdfWith d f vs g)

-- | Directed depth-first forest.
dff :: forall gr a b. Graph gr => List Node -> gr a b -> List (Tree Node)
dff = dffWith _.node

dffWith :: forall gr a b c. Graph gr => CFun a b c -> List Node -> gr a b -> List (Tree c)
dffWith = xdffWith (Graph.suc' >>> List.fromFoldable)

dffAllWith :: forall gr a b c. Graph gr => CFun a b c -> gr a b -> List (Tree c)
dffAllWith f = withAllNodes (dffWith f)

dffAll :: forall gr a b. Graph gr => gr a b -> List (Tree Node)
dffAll = dffAllWith _.node

-- | Undirected depth-first forest.
undirectedDff :: forall gr a b. Graph gr => List Node -> gr a b -> List (Tree Node)
undirectedDff = undirectedDffWith _.node

undirectedDffWith :: forall gr a b c. Graph gr => CFun a b c -> List Node -> gr a b -> List (Tree c)
undirectedDffWith = xdffWith (Graph.neighbors' >>> List.fromFoldable)

undirectedDffAllWith :: forall gr a b c. Graph gr => CFun a b c -> gr a b -> List (Tree c)
undirectedDffAllWith f = withAllNodes (undirectedDffWith f)

undirectedDffAll :: forall gr a b. Graph gr => gr a b -> List (Tree Node)
undirectedDffAll = undirectedDffAllWith _.node

-- | Reverse depth-first forest.
revDff :: forall gr a b. Graph gr => List Node -> gr a b -> List (Tree Node)
revDff = revDffWith _.node

revDffWith :: forall gr a b c. Graph gr => CFun a b c -> List Node -> gr a b -> List (Tree c)
revDffWith = xdffWith (Graph.pre' >>> List.fromFoldable)

revDffAllWith :: forall gr a b c. Graph gr => CFun a b c -> gr a b -> List (Tree c)
revDffAllWith f = withAllNodes (revDffWith f)

revDffAll :: forall gr a b. Graph gr => gr a b -> List (Tree Node)
revDffAll = revDffAllWith _.node


----------------------------------------------------------------------
-- ALGORITHMS BASED ON DFS
----------------------------------------------------------------------

-- | Collection of connected components
connectedComponents :: forall gr a b. Graph gr => gr a b -> List (List Node)
connectedComponents = map Graph.preorder <<< undirectedDffAll

-- | Number of connected components
numConnectedComponents :: forall gr a b. Graph gr => gr a b -> Int
numConnectedComponents = List.length <<< connectedComponents

-- | Is the graph connected?
isConnected :: forall gr a b. Graph gr => gr a b -> Boolean
isConnected = (_ == 1) <<< numConnectedComponents

-- | Flatten a 'Tree' in reverse order
postflatten :: forall a. Tree a -> List a
postflatten t = postflattenForest (Cofree.tail t) <> List.singleton (Cofree.head t)

-- | Flatten a forest in reverse order
postflattenForest :: forall a. List (Tree a) -> List a
postflattenForest ts = List.concatMap postflatten ts

-- | <http://en.wikipedia.org/wiki/Topological_sorting Topological sorting>,
--   i.e. a list of 'Node's so that if there's an edge between a source and a
--   target node, the source appears earlier in the result.
topologicalSort :: forall gr a b. Graph gr => gr a b -> List Node
topologicalSort = List.reverse <<< postflattenForest <<< dffAll

-- | 'topsort', returning only the labels of the nodes.
topologicalSort' :: forall gr a b. Graph gr => gr a b -> List a
topologicalSort' = List.reverse <<< Graph.postorderForest <<< dffAllWith _.label

-- | Collection of strongly connected components
stronglyConnectedComponents :: forall gr a b. Graph gr => gr a b -> List (List Node)
stronglyConnectedComponents g = map Graph.preorder (revDff (topologicalSort g) g)

-- | Collection of nodes reachable from a starting point.
reachable :: forall gr a b. Graph gr => Node -> gr a b -> List Node
reachable v g = Graph.preorderForest (dff (List.singleton v) g)

-- | The condensation of the given graph, i.e., the graph of its
-- strongly connected components.
condensation :: forall gr a b. Graph gr => gr a b -> gr (List Node) Unit
condensation gr = unsafePartial $ fromRight $ Graph.mkGraph vs es
  where
    sccs :: List (List Node)
    sccs = stronglyConnectedComponents gr

    vs :: List (Tuple Int (List Node))
    vs = List.fromFoldable $
           LazyList.zip (LazyList.iterate (_ + 1) 1) (LazyList.fromFoldable sccs)

    vMap :: Map (List Node) Int
    vMap = Map.fromFoldable $ map Tuple.swap vs

    getN :: List Node -> Int
    getN = (vMap `unsafeLookup` _)

    es = do
      c1 <- sccs
      c2 <- sccs
      guard (c1 /= c2 && anyHasEdge c1 c2)
      pure $ Tuple {from: getN c1, to: getN c2} unit

    anyHasEdge c1 c2 = List.any (Graph.hasEdge gr) do
      x <- c1
      y <- c2
      pure { from: x, to: y }

    unsafeLookup m k = unsafePartial (fromJust (Map.lookup k m))
