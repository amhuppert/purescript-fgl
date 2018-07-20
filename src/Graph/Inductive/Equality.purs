module Graph.Inductive.Equality
       (equal) where

import Prelude

import Data.Function (on)
import Graph.Inductive.Class (class Graph)
import Graph.Inductive.Class (labEdges, labNodes) as Graph
import Graph.Inductive.Core (labelEdge) as Graph
import Graph.Inductive.Types (Edge(..), LEdge(..), LNode)
import Graph.Inductive.Types.Accessors as A
import Data.List (List)
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmptyList


equal :: forall gr k a b. Ord k => Ord k => Eq a => Eq b => Graph gr => gr k a b -> gr k a b -> Boolean
equal g g' = sortedLNodes g == sortedLNodes g' && groupedLEdges g == groupedLEdges g'
-- This assumes that nodes aren't repeated (which shouldn't happen for
-- sane graph instances).  If node IDs are repeated, then the usage of
-- sortedLNodes cannot guarantee stable ordering.

-- Newtype wrapper just to test for equality of multiple edges.  This
-- is needed because without an Ord constraint on `b' it is not
-- possible to guarantee a stable ordering on edge labels.
newtype GroupEdges k b = GEs (LEdge k (NonEmptyList b))

sortedLNodes :: forall gr k a b. Ord k => Graph gr => gr k a b -> List (LNode k a)
sortedLNodes = List.sortBy (compare `on` A.nodeFromLNode) <<< Graph.labNodes

groupedLEdges :: forall gr k a b. Ord k => Ord k => Graph gr => gr k a b -> List (GroupEdges k b)
groupedLEdges = map (GEs <<< groupLabels)
            <<< List.groupBy (eqEdge `on` A.edgeFromLEdge)
            <<< List.sortBy (compareEdge `on` A.edgeFromLEdge)
            <<< Graph.labEdges
  where
    groupLabels les = Graph.labelEdge (A.edgeFromLEdge (head les)) (map A.labelFromLEdge les)
    head = NonEmptyList.head
    eqEdge (Edge a) (Edge b) = a.from == b.from && a.to == b.to
    compareEdge (Edge a) (Edge b) =
      case compare a.from b.from of
        EQ -> compare a.to b.to
        ne -> ne

instance eqGroupEdges :: (Eq b, Ord k) => Eq (GroupEdges k b) where
  eq (GEs (LEdge { edge: e1, label: l1 })) (GEs (LEdge { edge: e2, label: l2 })) =
      e1 == e2
      && eqList (NonEmptyList.toList l1) (NonEmptyList.toList l2)

-- OK to use \\ here as we want each value in xs to cancel a *single*
-- value in ys.
eqList :: forall a. Eq a => List a -> List a -> Boolean
eqList xs ys = List.null (xs List.\\ ys) && List.null (ys List.\\ xs)
