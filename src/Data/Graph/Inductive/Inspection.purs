module Data.Graph.Inductive.Inspection where

import Prelude

import Data.Array as Array
import Data.Graph.Inductive.Core (class Graph, Adj, Context, Edge, LEdge, LNode, Node, projectContextPred', projectContextSucc, projectContextSucc', (.:))
import Data.Graph.Inductive.Core as Core
import Data.Graph.Inductive.Inspection as Core
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple as Tuple

-- | True if the Node is present in the Graph
elem :: forall gr a b. Graph gr => Node -> gr a b -> Boolean
elem n = Core.match n >>> isJust

nodeLabel :: forall gr a b. Graph gr => gr a b -> Node -> Maybe a
nodeLabel g n = _.label <$> Core.context g n

lneighbors :: forall gr a b. Graph gr => gr a b -> Node -> Adj b
lneighbors g n = fromMaybe [] (neighbors' <$> Core.context g n)
  where neighbors' c = c.incomers <> c.outgoers

neighbors :: forall gr a b. Graph gr => gr a b -> Node -> Array Node
neighbors g n = fromMaybe [] (neighbors' <$> Core.context g n)
  where neighbors' c = map snd c.incomers <> map snd c.outgoers

suc :: forall gr a b. Graph gr => gr a b -> Node -> Array Node
suc = map snd .: Core.projectContextSucc

pre :: forall gr a b. Graph gr => gr a b -> Node -> Array Node
pre = map snd .: Core.projectContextPred

lsuc :: forall gr a b. Graph gr => gr a b -> Node -> Array (Tuple Node b)
lsuc = map Tuple.swap .: Core.projectContextSucc

lpre :: forall gr a b. Graph gr => gr a b -> Node -> Array (Tuple Node b)
lpre = map Tuple.swap .: Core.projectContextPred

out :: forall gr a b. Graph gr => gr a b -> Node -> Array (LEdge b)
out g v = map packEdge $ Core.projectContextSucc g v
  where packEdge (Tuple label to) = Tuple { from: v, to } label

inn :: forall gr a b. Graph gr => gr a b -> Node -> Array (LEdge b)
inn g v = map packEdge $ Core.projectContextPred g v
  where packEdge (Tuple label from) = Tuple { from, to: v } label

outdeg :: forall gr a b. Graph gr => gr a b -> Node -> Int
outdeg = Array.length .: Core.projectContextSucc

indeg :: forall gr a b. Graph gr => gr a b -> Node -> Int
indeg = Array.length .: Core.projectContextPred

-- | The degree of the 'Node'.
deg :: forall gr a b. Graph gr => gr a b -> Node -> Int
deg gr v = maybe 0 deg' $ Core.context gr v

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
