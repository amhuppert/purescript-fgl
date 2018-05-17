module Data.Graph.Inductive.Core
  ( Node
  , Edge
  , Path
  , LNode
  , LEdge
  , Adj
  , Context
  , Decomp
  , UGraph
  , class Graph
  , empty
  , isEmpty
  , match
  , matchAny
  , labNodes
  , labEdges
  , mkGraph
  , order
  , nodeRange
  , class DynGraph
  , merge
  , (&)
  , fold
  , mapContexts
  , mapNodes
  , mapEdges
  , mapNodesEdges
  , unsafeMerge
  , unlabelEdge
  , unlabelNode
  , labelEdge
  , labelNode
  , context
  , projectContextSucc
  , projectContextPred
  , compose2
  , (.:)
  , equal
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either, fromRight)
import Data.Foldable (class Foldable)
import Data.Function (on)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.NonEmpty (NonEmpty)
import Data.NonEmpty as NonEmpty
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafePartial)

type Node = Int

type Edge = { from :: Node
            , to   :: Node
            }

type Path = List Node

type LNode a = Tuple Node a

type LEdge b = Tuple Edge b

type Adj b = Array (Tuple b Node)

type Context a b = { incomers :: Adj b
                   , node :: Node
                   , label :: a
                   , outgoers :: Adj b
                   }

type Decomp gr a b = { context :: (Context a b)
                     , remaining :: gr a b
                     }

type UGraph gr = gr Unit Unit

unlabelEdge :: forall b. LEdge b -> Edge
unlabelEdge = fst

unlabelNode :: forall a. LNode a -> Node
unlabelNode = fst

labelEdge :: forall b. Edge -> b -> LEdge b
labelEdge e lab = Tuple e lab

labelNode :: forall a. Node -> a -> LNode a
labelNode n lab = Tuple n lab

-- | A Graph parameterized by the types of the vertex and edge labels.
class Graph gr where
  -- | An empty Graph
  empty :: forall a b. gr a b

  -- | True if the given Graph is empty
  isEmpty :: forall a b. gr a b -> Boolean

  -- | Decompose a Graph into the context of the given node (if it exists) and the remaining graph.
  match :: forall a b. Node -> gr a b -> Maybe (Decomp gr a b)

  -- | Create a Graph from the provided nodes and edges
  mkGraph :: forall f a b. Foldable f => f (LNode a) -> f (LEdge b) -> gr a b

  -- | Get a list of all LNodes in the Graph
  labNodes :: forall a b. gr a b -> Array (LNode a)

  -- | Decompose a graph into a context for an arbitrarily chosen node and the remaining Graph
  matchAny :: forall a b. gr a b -> Maybe (Decomp gr a b)

  -- | The number of nodes in a Graph
  order :: forall a b. gr a b -> Int

  -- | The minimum and maximum node in a Graph
  nodeRange :: forall a b. gr a b -> Maybe { min :: Node, max :: Node }

  -- | A list of all labeled edges in the Graph
  labEdges :: forall a b. gr a b -> Array (LEdge b)


class Graph gr <= DynGraph gr where
  -- | Merge a context into an existing Graph.
  --   The edges should only refer to nodes that exist in the graph or to the node being merged.
  merge :: forall a b. Context a b -> gr a b -> Either String (gr a b)

infixr 8 merge as &

newNodes :: forall gr a b. Graph gr => Int -> gr a b -> List Node
newNodes count graph
  | isEmpty graph = 0 List...  (count - 1)
  | otherwise = let { max } = unsafePartial (fromJust (nodeRange graph))
                 in (max + 1) List... (max + count)

numEdges :: forall gr a b. (Graph gr) => gr a b -> Int
numEdges = labEdges >>> Array.length

unsafeMerge :: forall gr a b. DynGraph gr => Context a b -> gr a b -> gr a b
unsafeMerge c gr = unsafePartial (fromRight (c & gr))

-- Graph folds and maps

-- | Fold a function over the Graph by recursively calling matchAny
fold :: forall gr a b c. Graph gr => (Context a b -> c -> c) -> c -> gr a b -> c
fold f accum graph
  | isEmpty graph = accum
  | otherwise =
      let { context, remaining } = unsafePartial (fromJust (matchAny graph))
          accum' = f context accum
       in fold f accum' remaining

mapContexts :: forall gr a b c d. DynGraph gr => (Context a b -> Context c d) -> gr a b -> gr c d
mapContexts f = fold (\c gr -> unsafeMerge (f c) gr) empty

mapNodes :: forall gr a b a'. DynGraph gr => (a -> a') -> gr a b -> gr a' b
mapNodes f = mapContexts (\c -> c { label = f c.label })

mapEdges :: forall gr a b b'. DynGraph gr => (b -> b') -> gr a b -> gr a b'
mapEdges f = mapContexts (\c -> c { incomers = mapAdj c.incomers
                                  , outgoers = mapAdj c.outgoers
                                  }
                         )
  where mapAdj = map (\(Tuple label n) -> Tuple (f label) n)

mapNodesEdges :: forall gr a b a' b'. DynGraph gr => (a -> a') -> (b -> b') -> gr a b -> gr a' b'
mapNodesEdges nodeMapper edgeMapper =
  mapContexts (\c -> c { incomers = mapAdj c.incomers
                       , label = nodeMapper c.label
                       , outgoers = mapAdj c.outgoers
                       }
              )
  where mapAdj = map (\(Tuple label n) -> Tuple (edgeMapper label) n)

nodes :: forall gr a b. Graph gr => gr a b -> Array Node
nodes = labNodes >>> map unlabelNode

edges :: forall gr a b. Graph gr => gr a b -> Array Edge
edges = labEdges >>> map unlabelEdge

---------------------------
-- utilities --------------
---------------------------

compose2 :: forall a b c d. (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 = compose <<< compose

infixr 8 compose2 as .:

-- projecting on context elements
--
projectContextSucc :: forall gr a b. Graph gr => gr a b -> Node -> Adj b
projectContextSucc = maybe [] projectContextSucc' .: context

projectContextPred :: forall gr a b. Graph gr => gr a b -> Node -> Adj b
projectContextPred = maybe [] projectContextPred' .: context

context :: forall gr a b. Graph gr => gr a b -> Node -> Maybe (Context a b)
context g n = match n g >>= (_.context >>> Just)

projectContextPred' :: forall a b. Context a b -> Adj b
projectContextPred' c = c.incomers <> selfLoops
  where selfLoops = Array.filter ((_ == c.node) <<< snd) c.outgoers

projectContextSucc' :: forall a b. Context a b -> Adj b
projectContextSucc' c = c.outgoers <> selfLoops
  where selfLoops = Array.filter ((_ == c.node) <<< snd) c.incomers

----------------------------------------------------------------------
-- GRAPH EQUALITY
----------------------------------------------------------------------

slabNodes :: forall gr a b. Graph gr => gr a b -> Array (LNode a)
slabNodes = Array.sortBy (compare `on` fst) <<< labNodes

glabEdges :: forall gr a b. Graph gr => gr a b -> Array (GroupEdges b)
glabEdges = map (GEs <<< groupLabels)
            <<< Array.groupBy (eqEdge `on` unlabelEdge)
            <<< Array.sortBy (compareEdge `on` unlabelEdge)
            <<< labEdges
  where
    groupLabels les = labelEdge (unlabelEdge (head les)) (map snd les)
    head = NonEmpty.head
    eqEdge a b = a.from == b.from && a.to == b.to
    compareEdge a b =
      case compare a.from b.from of
        EQ -> compare a.to b.to
        ne -> ne

equal :: forall gr a b. Eq a => Eq b => Graph gr => gr a b -> gr a b -> Boolean
equal g g' = slabNodes g == slabNodes g' && glabEdges g == glabEdges g'
-- This assumes that nodes aren't repeated (which shouldn't happen for
-- sane graph instances).  If node IDs are repeated, then the usage of
-- slabNodes cannot guarantee stable ordering.

-- Newtype wrapper just to test for equality of multiple edges.  This
-- is needed because without an Ord constraint on `b' it is not
-- possible to guarantee a stable ordering on edge labels.
newtype GroupEdges b = GEs (LEdge (NonEmpty Array b))

instance eqGroupEdges :: (Eq b) => Eq (GroupEdges b) where
  eq (GEs (Tuple e1 ls1)) (GEs (Tuple e2 ls2)) =
      e1.from == e2.from
      && e1.to == e2.to
      && eqArr (fromNE ls1) (fromNE ls2)
    where fromNE = NonEmpty.fromNonEmpty (\head tail -> head Array.: tail)

eqArr :: forall a. Eq a => Array a -> Array a -> Boolean
eqArr xs ys = Array.null (xs Array.\\ ys) && Array.null (ys Array.\\ xs)
-- OK to use \\ here as we want each value in xs to cancel a *single*
-- value in ys.
