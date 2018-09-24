module Graph.Inductive.Class where

import Prelude

import Graph.Inductive.Types (Context, Edge, EdgeContext, GraphDecomposition, LEdge, LNode)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Traversable (class Traversable)

-- | A Graph parameterized by the types of the node id type and vertex/edge labels.
class Graph gr where
  -- | An empty Graph
  empty :: forall k a b. gr k a b

  -- | True if the given Graph is empty
  isEmpty :: forall k a b. gr k a b -> Boolean

  -- | Decompose a Graph into the context of the given node (if it exists) and the remaining graph.
  match :: forall k a b. Ord k => k -> gr k a b -> Maybe (GraphDecomposition gr k a b)

  -- | Decompose a graph into a context for an arbitrarily chosen node and the remaining Graph
  matchAny :: forall k a b. Ord k => gr k a b -> Maybe (GraphDecomposition gr k a b)

  -- | Create a Graph from the provided nodes and edges
  mkGraph :: forall f k a b. Ord k => Traversable f => f (LNode k a) -> f (LEdge k b) -> gr k a b

  -- | Get a list of all LNodes in the Graph
  labNodes :: forall k a b. gr k a b -> List (LNode k a)

  -- | A list of all labeled edges in the Graph
  labEdges :: forall k a b. gr k a b -> List (LEdge k b)

  -- | The number of nodes in a Graph
  order :: forall k a b. gr k a b -> Int

  edgeContext :: forall k a b. Ord k => Edge k -> gr k a b -> Maybe (EdgeContext k a b)


class Graph gr <= OrdGraph gr where
  -- | The maximum node in a Graph
  maxNode :: forall k a b. Ord k => gr k a b -> Maybe k
  -- | The minimum node in a Graph
  minNode :: forall k a b. Ord k => gr k a b -> Maybe k

class Graph gr <= DynGraph gr where
  -- | Merge a context into an existing Graph.
  -- | The incident edges within the context **must** only refer to nodes that already exist in the graph or to the node being merged.
  merge :: forall k a b. Ord k => Context k a b -> gr k a b -> gr k a b

  updateNode :: forall k a b. Ord k => (a -> a) -> k -> gr k a b -> gr k a b

  updateEdge :: forall k a b. Ord k => (b -> b) -> Edge k -> gr k a b -> gr k a b

  -- | Transform the labels for all nodes in the Graph.
  mapNodesWithKey :: forall k a b a'. (k -> a -> a') -> gr k a b -> gr k a' b


infixr 8 merge as &
