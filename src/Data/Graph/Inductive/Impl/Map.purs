-- | Graph/DynGraph implementation using Data.Map
module Data.Graph.Inductive.Impl.Map
  ( Gr
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, foldr)
import Data.Graph.Inductive.Core (class DynGraph, class Graph, Adj, Decomp, Edge, LEdge, Node, EdgeContext, insEdges, match)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

type AdjMap b = Map Node (Array b)

type Context' a b =
  { incomers :: AdjMap b
  , label :: a
  , outgoers :: AdjMap b
  }

newtype Gr a b = Gr (GrRep a b)

type GrRep a b = Map Node (Context' a b)

instance grMapGraph :: Graph Gr where
  empty = Gr (Map.empty)

  isEmpty (Gr g) = Map.isEmpty g

  match = matchGr

  mkGraph vs es = vs # map (second mkIsolatedContext)
                  >>> Map.fromFoldable
                  >>> Gr
                  >>> insEdges es
    where mkIsolatedContext label = { incomers: Map.empty, label, outgoers: Map.empty }

  labNodes (Gr g) = g # Map.toUnfoldable >>> map (\(Tuple v c) -> Tuple v c.label)

  matchAny (Gr g) = do
    { key } <- Map.findMin g
    match key (Gr g)

  order (Gr g) = Map.size g

  nodeRange (Gr g) = do
    { key: min } <- Map.findMin g
    { key: max } <- Map.findMax g
    Just { min, max }

  labEdges (Gr g) = g # Map.toUnfoldable
                    >>> Array.concatMap toLEdges
    where toLEdges :: forall a b. Tuple Node (Context' a b) -> Array (LEdge b)
          toLEdges (Tuple from c) = Array.concatMap (adjsToLEdge from) (Map.toUnfoldable c.outgoers)
          adjsToLEdge :: forall a b. Node -> Tuple Node (Array b) -> Array (LEdge b)
          adjsToLEdge from (Tuple to labels) = map (\l -> Tuple { from, to } l) labels

  edgeContext = edgeContextImpl

second :: forall a b c. (b -> c) -> Tuple a b -> Tuple a c
second f (Tuple a b) = Tuple a (f b)

matchGr :: forall a b. Node -> Gr a b -> Maybe (Decomp Gr a b)
matchGr targetNode (Gr g) = case Map.lookup targetNode g of
  Nothing -> Nothing
  Just c ->
    let c' = { incomers: toAdj (Map.delete targetNode c.incomers)
             , node: targetNode
             , label: c.label
             , outgoers: toAdj c.outgoers
             }
        g' = g # Map.delete targetNode >>> clearSuccs c >>> clearPreds c
     in Just { context: c', remaining: Gr g' }

  where clearSuccs c gr = foldr rm gr (Map.keys c.outgoers)
          where rm removeFrom = rmIncomer {removeFrom, toRemove: targetNode}
        clearPreds c gr = foldr rm gr (Map.keys c.incomers)
          where rm removeFrom = rmOutgoer {removeFrom, toRemove: targetNode}

toAdj :: forall b. AdjMap b -> Adj b
toAdj am = Array.concatMap expand adjs
  where expand :: Tuple Node (Array b) -> Array (Tuple b Node)
        expand (Tuple v ls) = map (\l -> Tuple l v) ls
        adjs :: Array (Tuple Node (Array b))
        adjs = Map.toUnfoldable am

fromAdj :: forall b. Adj b -> AdjMap b
fromAdj = foldl insertAdj Map.empty

insertAdj :: forall b. AdjMap b -> Tuple b Node -> AdjMap b
insertAdj g (Tuple label v) = Map.alter alter v g
    where alter = case _ of
            Nothing -> Just [label]
            Just ls -> Just $ ls `Array.snoc` label

instance grMapDynGraph :: DynGraph Gr where
  merge c (Gr g) =
      let g1 = Map.insert c.node { incomers: fromAdj c.incomers
                                 , label: c.label
                                 , outgoers: fromAdj c.outgoers
                                 } g
          g2 = g1 # addSucc >>> addPred
       in Gr g2
    where addSucc graph = foldr (addEdge addIncomer) graph c.outgoers
          addPred graph = foldr (addEdge addOutgoer) graph c.incomers
          addEdge add (Tuple label node) =
            add { toAdd: { label, node: c.node }
                , addTo: node
                }

type RmParams = { toRemove :: Node
                , removeFrom :: Node
                }

rmOutgoer :: forall a b. RmParams -> GrRep a b -> GrRep a b
rmOutgoer {toRemove,removeFrom} g = Map.update rmOutgoer' removeFrom g
  where rmOutgoer' c = Just $ c { outgoers = Map.delete toRemove c.outgoers }

rmIncomer :: forall a b. RmParams -> GrRep a b -> GrRep a b
rmIncomer {toRemove,removeFrom} g = Map.update rmIncomer' removeFrom g
  where rmIncomer' c = Just $ c { incomers = Map.delete toRemove c.incomers }

type AddParams b = { toAdd :: { label :: b
                              , node :: Node
                              }
                   , addTo :: Node
                   }

addIncomer :: forall a b. AddParams b -> GrRep a b -> GrRep a b
addIncomer {toAdd,addTo} g = Map.update doAdd addTo g
  where doAdd c = Just $ c { incomers = Map.alter alter toAdd.node c.incomers }
        alter = case _ of
          Nothing -> Just [toAdd.label]
          Just es -> Just $ es `Array.snoc` toAdd.label

addOutgoer :: forall a b. AddParams b -> GrRep a b -> GrRep a b
addOutgoer {toAdd,addTo} g = Map.update doAdd addTo g
  where doAdd c = Just $ c { outgoers = Map.alter alter toAdd.node c.outgoers }
        alter = case _ of
          Nothing -> Just [toAdd.label]
          Just es -> Just $ es `Array.snoc` toAdd.label

edgeContextImpl :: forall a b. Edge -> Gr a b -> Maybe (EdgeContext a b)
edgeContextImpl edge (Gr graphRep) = do
  source <- Map.lookup edge.from graphRep
  target <- Map.lookup edge.to graphRep
  edgeLabels <- Map.lookup edge.to source.outgoers
  Just { edgeLabels
       , source: Tuple edge.from source.label
       , target: Tuple edge.to target.label
       }
