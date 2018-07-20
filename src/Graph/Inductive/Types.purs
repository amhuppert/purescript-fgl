module Graph.Inductive.Types where


import Prelude

import Data.Lazy (Lazy)
import Data.List (List)
import Data.Newtype (class Newtype)

newtype Edge k = Edge (EdgeRec k)
derive newtype instance showEdge :: Show k => Show (Edge k)
  -- show (Edge { from, to }) =
  --   "Edge { from: \"" <> show from <> "\", to: \"" <> show to <> "\" }"
derive instance eqEdge :: Eq k => Eq (Edge k)
derive instance newtypeEdge :: Newtype (Edge k) _
derive instance ordEdge :: Ord k => Ord (Edge k)

type EdgeRec k = { from :: k
                 , to   :: k
                 }

newtype Path k = Path (List k)
derive instance eqPath :: Eq k => Eq (Path k)
derive instance ordPath :: Ord k => Ord (Path k)
derive instance newtypePath :: Newtype (Path k) _
derive newtype instance showPath :: Show k => Show (Path k)

newtype LNode k a = LNode (LNodeRec k a)
derive instance eqLNode :: (Eq k, Eq a) => Eq (LNode k a)
derive instance newtypeLNode :: Newtype (LNode k a) _
derive newtype instance showLNode :: (Show k, Show a) => Show (LNode k a)

type LNodeRec k a = { node :: k
                    , label :: a
                    }

newtype LEdge k b = LEdge (LEdgeRec k b)
derive instance eqLEdge :: (Eq k, Eq b) => Eq (LEdge k b)
derive instance newtypeLEdge :: Newtype (LEdge k b) _
derive newtype instance showLEdge :: (Show k, Show b) => Show (LEdge k b)

type LEdgeRec k b = { edge :: Edge k
                    , label :: b
                    }

type IncidentEdges k b = List (IncidentEdge k b)

newtype IncidentEdge k b = IncidentEdge (IncidentEdgeRec k b)
derive instance eqIncidentEdge :: (Eq k, Eq b) => Eq (IncidentEdge k b)
derive instance newtypeIncidentEdge :: Newtype (IncidentEdge k b) _
derive newtype instance showIncidentEdge :: (Show k, Show b) => Show (IncidentEdge k b)

type IncidentEdgeRec k b = { label :: b
                           , node :: k
                           }

newtype Context k a b = Context (ContextRec k a b)
derive instance eqContext :: (Eq k, Eq a, Eq b) => Eq (Context k a b)
derive instance newtypeContext :: Newtype (Context k a b) _
derive newtype instance showContext :: (Show k, Show a, Show b) => Show (Context k a b)

type ContextRec k a b =
  { incomers :: IncidentEdges k b
  , node :: k
  , label :: a
  , outgoers :: IncidentEdges k b
  }

newtype GraphDecomposition gr k a b = Decomp (DecompRec gr k a b)
derive instance newtypeGraphDecomp :: Newtype (GraphDecomposition gr k a b) _
derive newtype instance showGraphDecomp :: (Show k, Show a, Show b, Show (gr k a b)) => Show (GraphDecomposition gr k a b)

type DecompRec gr k a b = { context :: (Context k a b)
                          , remaining :: Lazy (gr k a b)
                          }

type UGraph gr k = gr k Unit Unit

newtype EdgeContext k a b = EdgeContext (EdgeContextRec k a b)
derive instance eqEdgeContext :: (Eq k, Eq a, Eq b) => Eq (EdgeContext k a b)
derive instance newtypeEdgeContext :: Newtype (EdgeContext k a b) _
derive newtype instance showEdgeContext :: (Show k, Show a, Show b) => Show (EdgeContext k a b)

type EdgeContextRec k a b =
  { edgeLabels :: List b
  , source :: LNode k a
  , target :: LNode k a
  }
