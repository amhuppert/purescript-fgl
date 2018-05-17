module Data.Graph.Inductive.Inspection where

import Prelude

import Data.Array as Array
import Data.Graph.Inductive.Core (class Graph, Adj, Context, Node, LEdge, projectContextSucc, (.:))
import Data.Graph.Inductive.Core as Core
import Data.Graph.Inductive.Inspection as Core
import Data.Maybe (Maybe(..), fromMaybe, isJust)
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
