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
  , newNodes
  , newNode
  , class DynGraph
  , merge
  , (&)
  , fold
  , mapContexts
  , mapNodes
  , mapEdges
  , mapNodesEdges
  , modifyNodeLabel
  , unsafeMerge
  , unlabelEdge
  , unlabelNode
  , labelEdge
  , labelNode
  , context
  , projectContextSucc
  , projectContextPred
  , projectContextSucc'
  , projectContextPred'
  , compose2
  , (.:)
  , equal
  , insNode
  , insNodes
  , insEdges
  , insEdge
  , delNodes
  , delNode
  , delEdge
  , delEdges
  , delLEdgeBy
  , delLEdges
  , buildGraph
  , mkUnlabeledGraph
  , elem
  , nodeLabel
  , lneighbors
  , lneighbors'
  , neighbors
  , suc
  , pre
  , lsuc
  , lpre
  , out
  , inn
  , outdeg
  , indeg
  , deg
  , hasEdge
  , hasNeighbor
  , hasLEdge
  , hasNeighborAdj
  , labNode'
  , neighbors'
  , suc'
  , pre'
  , lpre'
  , out'
  , inn'
  , outdeg'
  , indeg'
  , deg'
  , filterMap
  , filterNodes
  , filterNodesOnLabel
  , filterLNodes
  , subgraph
  , filterEdges
  , filterEdgesOnLabel
  , nodes
  , contexts
  , postorder
  , postorderForest
  , preorder
  , preorderForest
  , edges
  ) where

import Prelude

import Control.Comonad.Cofree as Cofree
import Data.Array as Array
import Data.Either (Either(..), fromRight)
import Data.Either as Either
import Data.Foldable (class Foldable, foldM, foldr)
import Data.Function (on)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust, maybe)
import Data.NonEmpty (NonEmpty)
import Data.NonEmpty as NonEmpty
import Data.Set as Set
import Data.Traversable (class Traversable)
import Data.Tree (Tree)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple as Tuple
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
  mkGraph :: forall f a b. Traversable f => f (LNode a) -> f (LEdge b) -> Either String (gr a b)

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

newNode :: forall gr a b. Graph gr => gr a b -> Node
newNode graph
  | isEmpty graph = 0
  | otherwise = let { max } = unsafePartial (fromJust (nodeRange graph))
                 in max + 1

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

modifyNodeLabel :: forall gr a b. DynGraph gr => (a -> a) -> Node -> gr a b -> gr a b
modifyNodeLabel f v g =
  case match v g of
    Nothing -> g
    Just { context, remaining } ->
      let c' = context { label = f context.label }
       in c' `unsafeMerge` remaining

contexts :: forall gr a b. Graph gr => gr a b -> Array (Context a b)
contexts g = Array.mapMaybe (flip match g >=> (_.context >>> Just)) (nodes g)

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

insNode :: forall gr a b. DynGraph gr => LNode a -> gr a b -> gr a b
insNode (Tuple node label) gr = unsafeMerge newContext gr
  where newContext = { incomers: [], node, label, outgoers: [] }

insNodes :: forall gr a b. DynGraph gr => Array (LNode a) -> gr a b -> gr a b
insNodes ns g = foldr insNode g ns

insEdge :: forall gr a b. DynGraph gr => LEdge b -> gr a b -> Either String (gr a b)
insEdge (Tuple edge label) gr =
  maybe
    (Left $ "insEdge: Cannot insert edge from non-existent vertex "
          <> show edge.from)
    (\c -> add c.context & c.remaining)
    source

  where source = match edge.from gr
        add c = c { outgoers = c.outgoers `Array.snoc` (Tuple label edge.to) }

insEdges :: forall gr a b f. DynGraph gr => Foldable f => f (LEdge b) -> gr a b -> Either String (gr a b)
insEdges es g = foldM (flip insEdge) g es

delNodes :: forall gr a b. DynGraph gr => Array Node -> gr a b -> gr a b
delNodes vs g = foldr rm g vs
  where rm v currG = case match v currG of
          Nothing -> currG
          Just { context, remaining } -> remaining

delNode :: forall gr a b. DynGraph gr => Node -> gr a b -> gr a b
delNode v = delNodes [v]

delEdge :: forall gr a b. DynGraph gr => Edge -> gr a b -> gr a b
delEdge e graph =
  case match e.from graph of
    Nothing -> graph
    Just { context: c, remaining } ->
      let c' = c { outgoers = Array.filter nEq c.outgoers }
       in c' `unsafeMerge` remaining
  where nEq (Tuple _ o) = o /= e.to

delEdges :: forall gr a b f. DynGraph gr => Foldable f => f Edge -> gr a b -> gr a b
delEdges es g = foldr delEdge g es

delLEdgeBy :: forall gr a b. DynGraph gr =>
              (Tuple b Node -> Adj b -> Adj b)
           -> LEdge b
           -> gr a b
           -> gr a b
delLEdgeBy f (Tuple e label) g =
  case match e.from g of
    Nothing -> g
    Just { context: c, remaining } ->
      c { outgoers = f (Tuple label e.to) c.outgoers } `unsafeMerge` remaining

-- | Delete all edges equal to the one specified
delLEdges :: forall gr a b. DynGraph gr => Eq b => LEdge b -> gr a b -> gr a b
delLEdges  = delLEdgeBy (Array.filter <<< ne)
  where ne (Tuple b1 n1) (Tuple b2 n2) = not (n1 == n2 && b1 == b2)

-- | Build a Graph from a list of Contexts.
--   The list should be in the order such that earlier Contexts depend upon later ones
buildGraph :: forall gr a b f. DynGraph gr => Foldable f
            => f (Context a b)
            -> Either String (gr a b)
buildGraph = foldM (flip merge) empty

mkUnlabeledGraph :: forall t gr. Traversable t => Graph gr
                    => t Node
                    -> t Edge
                    -> Either String (UGraph gr)
mkUnlabeledGraph nodes edges =
  let le = map (_ `labelEdge` unit) edges
      ln = map (_ `labelNode` unit) nodes
   in mkGraph ln le

----------------------------------------------------------------
-- GRAPH INSPECTION
----------------------------------------------------------------

-- | True if the Node is present in the Graph
elem :: forall gr a b. Graph gr => Node -> gr a b -> Boolean
elem n = match n >>> isJust

nodeLabel :: forall gr a b. Graph gr => gr a b -> Node -> Maybe a
nodeLabel g n = _.label <$> context g n

lneighbors :: forall gr a b. Graph gr => gr a b -> Node -> Adj b
lneighbors g n = fromMaybe [] (neighbors' <$> context g n)
  where neighbors' c = c.incomers <> c.outgoers

neighbors :: forall gr a b. Graph gr => gr a b -> Node -> Array Node
neighbors g n = fromMaybe [] (neighbors' <$> context g n)
  where neighbors' c = map snd c.incomers <> map snd c.outgoers

suc :: forall gr a b. Graph gr => gr a b -> Node -> Array Node
suc = map snd .: projectContextSucc

pre :: forall gr a b. Graph gr => gr a b -> Node -> Array Node
pre = map snd .: projectContextPred

lsuc :: forall gr a b. Graph gr => gr a b -> Node -> Array (Tuple Node b)
lsuc = map Tuple.swap .: projectContextSucc

lpre :: forall gr a b. Graph gr => gr a b -> Node -> Array (Tuple Node b)
lpre = map Tuple.swap .: projectContextPred

out :: forall gr a b. Graph gr => gr a b -> Node -> Array (LEdge b)
out g v = map packEdge $ projectContextSucc g v
  where packEdge (Tuple label to) = Tuple { from: v, to } label

inn :: forall gr a b. Graph gr => gr a b -> Node -> Array (LEdge b)
inn g v = map packEdge $ projectContextPred g v
  where packEdge (Tuple label from) = Tuple { from, to: v } label

outdeg :: forall gr a b. Graph gr => gr a b -> Node -> Int
outdeg = Array.length .: projectContextSucc

indeg :: forall gr a b. Graph gr => gr a b -> Node -> Int
indeg = Array.length .: projectContextPred

-- | The degree of the 'Node'.
deg :: forall gr a b. Graph gr => gr a b -> Node -> Int
deg gr v = maybe 0 deg' $ context gr v

-- | Checks if there is a directed edge between two nodes.
hasEdge :: forall gr a b. Graph gr => gr a b -> Edge -> Boolean
hasEdge gr e = e.to `Array.elem` suc gr e.from

-- | Checks if there is an undirected edge between two nodes.
hasNeighbor :: forall gr a b. Graph gr => gr a b -> Node -> Node -> Boolean
hasNeighbor gr v w = w `Array.elem` neighbors gr v

-- | Checks if there is a labelled edge between two nodes.
hasLEdge :: forall gr a b. Graph gr => Eq b => gr a b -> LEdge b -> Boolean
hasLEdge gr (Tuple e l) = Tuple e.to l `Array.elem` lsuc gr e.from

-- | Checks if there is an undirected labelled edge between two nodes.
hasNeighborAdj :: forall gr a b. Graph gr => Eq b
                  => gr a b
                  -> Node
                  -> Tuple b Node
                  -> Boolean
hasNeighborAdj gr v a = a `Array.elem` lneighbors gr v

----------------------------------------------------
-- Context Inspection ------------------------------
----------------------------------------------------

-- | The 'LNode' from a 'Context'.
labNode' :: forall a b. Context a b -> LNode a
labNode' c = Tuple c.node c.label

-- | All 'Node's linked to or from in a 'Context'.
neighbors' :: forall a b. Context a b -> Array Node
neighbors' c = map snd c.incomers <> map snd c.outgoers

-- | All labelled links coming into or going from a 'Context'.
lneighbors' :: forall a b. Context a b -> Adj b
lneighbors' c = c.incomers <> c.outgoers

-- | All 'Node's linked to in a 'Context'.
suc' :: forall a b. Context a b -> Array (Node)
suc' = map snd <<< projectContextSucc'

-- | All 'Node's linked from in a 'Context'.
pre' :: forall a b. Context a b -> Array Node
pre' = map snd <<< projectContextPred'

-- | All 'Node's linked from in a 'Context', and the label of the links.
lsuc' :: forall a b. Context a b -> Array (Tuple Node b)
lsuc' = map Tuple.swap <<< projectContextSucc'

-- | All 'Node's linked from in a 'Context', and the label of the links.
lpre' :: forall a b. Context a b -> Array (Tuple Node b)
lpre' = map Tuple.swap <<< projectContextPred'

-- | All outward-directed 'LEdge's in a 'Context'.
out' :: forall a b. Context a b -> Array (LEdge b)
out' c = map (\(Tuple l to) -> Tuple { from: c.node, to } l) (projectContextSucc' c)

-- | All inward-directed 'LEdge's in a 'Context'.
inn' :: forall a b. Context a b -> Array (LEdge b)
inn' c = map (\(Tuple l from) -> Tuple { from, to: c.node } l) (projectContextPred' c)

-- | The outward degree of a 'Context'.
outdeg' :: forall a b. Context a b -> Int
outdeg' = Array.length <<< projectContextSucc'

-- | The inward degree of a 'Context'.
indeg' :: forall a b. Context a b -> Int
indeg' = Array.length <<< projectContextPred'

-- | The degree of a 'Context'.
deg' :: forall a b. Context a b -> Int
deg' c = Array.length c.incomers + Array.length c.outgoers

--------------------------------------------------------
-- SUBGRAPHS
--------------------------------------------------------

filterMap :: forall gr a b c d. DynGraph gr
          => (Context a b -> Maybe (Context c d))
          -> gr a b
          -> gr c d
filterMap f = fold foldFunc empty
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
  delNodes (map fst <<< Array.filter (not <<< p) $ labNodes gr) gr

-- | Returns the subgraph induced by the supplied nodes
subgraph :: forall gr a b f. DynGraph gr => Foldable f
         => f Node
         -> gr a b
         -> gr a b
subgraph vs = let vs'= Set.fromFoldable vs
               in filterNodes (_ `Set.member` vs')

-- | Filter based on edge property.
filterEdges :: forall gr a b. DynGraph gr => (LEdge b -> Boolean) -> gr a b -> gr a b
filterEdges f = fold cfilter empty
  where cfilter c g = c { incomers = incomers
                        , outgoers = outgoers
                        }
                      `unsafeMerge` g
          where incomers = Array.filter (\(Tuple b u) -> f $ Tuple { from: u, to: c.node } b) c.incomers
                outgoers = Array.filter (\(Tuple b w) -> f $ Tuple { from: c.node, to: w } b) c.outgoers

-- | Filter based on edge label property.
filterEdgesOnLabel :: forall gr a b. DynGraph gr => (b -> Boolean) -> gr a b -> gr a b
filterEdgesOnLabel f = filterEdges (\(Tuple _ b) -> f b)


-------------------------------------------------------
--- TREE OPERATIONS -----------------------------------
-------------------------------------------------------

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
  where squish t xs =
          Cofree.head t `Cons` foldr squish xs (Cofree.tail t)

-- | Flatten multiple 'Tree's in pre-order.
preorderForest :: forall a. List (Tree a) -> List a
preorderForest = List.concatMap preorder
