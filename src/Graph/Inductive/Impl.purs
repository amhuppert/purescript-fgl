-- | Graph/DynGraph implementation using Data.Map
module Graph.Inductive.Impl
  ( Gr
  ) where

import Prelude

import Data.Foldable (foldl, foldr)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lazy as Lazy
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Graph.Inductive.Class (class DynGraph, class Graph, class OrdGraph, match)
import Graph.Inductive.Core (insEdges)
import Graph.Inductive.Types (Context(..), Edge(..), EdgeContext(..), GraphDecomposition(..), IncidentEdge(..), IncidentEdges, LEdge(..), LNode(..))

type AdjMap k b = Map k (List b)

newtype Context' k a b = Context' { incomers :: AdjMap k b
                                  , label :: a
                                  , outgoers :: AdjMap k b
                                  }
derive instance eqContext' :: (Eq k, Eq a, Eq b) => Eq (Context' k a b)
derive newtype instance showContext' :: (Show k, Show a, Show b) => Show (Context' k a b)

newtype Gr k a b = Gr (GrRep k a b)

type GrRep k a b = Map k (Context' k a b)

instance grMapGraph :: Graph Gr where
  empty = Gr (Map.empty)

  isEmpty (Gr g) = Map.isEmpty g

  match = matchGr

  mkGraph vs es = vs # mkEdgelessGraph
                  >>> Gr
                  >>> insEdges es
    where mkEdgelessGraph = map lnodeToKeyVal
                            >>> Map.fromFoldable
          lnodeToKeyVal (LNode {node,label}) = Tuple node (mkIsolatedContext label)
          mkIsolatedContext label = Context' { incomers: Map.empty, label, outgoers: Map.empty }

  labNodes (Gr g) = g # Map.toUnfoldable
                    >>> map (\(Tuple v (Context' c)) -> LNode { node: v, label: c.label })

  matchAny (Gr g) = do
    { key } <- Map.findMin g
    match key (Gr g)

  order (Gr g) = Map.size g

  labEdges (Gr g) = g # Map.toUnfoldable
                    >>> List.concatMap toLEdges
    where toLEdges :: forall k a b. Tuple k (Context' k a b) -> List (LEdge k b)
          toLEdges (Tuple from (Context' c)) = List.concatMap (adjsToLEdge from)
                                                              (Map.toUnfoldable c.outgoers)
          adjsToLEdge :: forall k a b. k -> Tuple k (List b) -> List (LEdge k b)
          adjsToLEdge from (Tuple to labels) = map (\l -> LEdge { edge: Edge { from, to }
                                                                , label: l
                                                                }
                                                   ) labels

  edgeContext = edgeContextImpl

-- | O[log(n) + d*log(n)] where d is the degree of the matched node.
matchGr :: forall k a b. Ord k => k -> Gr k a b -> Maybe (GraphDecomposition Gr k a b)
matchGr targetNode (Gr g) = case Map.lookup targetNode g of
  Nothing -> Nothing
  Just (Context' c) ->
    let c' = { incomers: toAdj c.incomers
             , node: targetNode
             , label: c.label
             , outgoers: toAdj c.outgoers
             }
        g' = Lazy.defer \_ ->
               g # Map.delete targetNode
               >>> clearSuccs c
               >>> clearPreds c
               >>> Gr
     in Just $ Decomp { context: Context c', remaining: g' }

  where clearSuccs c gr = foldr rm gr (Map.keys c.outgoers)
          where rm removeFrom = rmIncomer {removeFrom, toRemove: targetNode}
        clearPreds c gr = foldr rm gr (Map.keys c.incomers)
          where rm removeFrom = rmOutgoer {removeFrom, toRemove: targetNode}

toAdj :: forall k b. AdjMap k b -> IncidentEdges k b
toAdj am = List.concatMap expand adjs
  where expand :: Tuple k (List b) -> IncidentEdges k b
        expand (Tuple node ls) = map (\label -> IncidentEdge { node, label }) ls
        adjs :: List (Tuple k (List b))
        adjs = Map.toUnfoldable am

fromAdj :: forall k b. Ord k => IncidentEdges k b -> AdjMap k b
fromAdj = foldl insertAdj Map.empty

insertAdj :: forall k b. Ord k => AdjMap k b -> IncidentEdge k b -> AdjMap k b
insertAdj g (IncidentEdge { node, label }) = Map.alter alter node g
    where alter = case _ of
            Nothing -> Just $ List.singleton label
            Just ls -> Just $ label `Cons` ls

instance grMapDynGraph :: DynGraph Gr where
  -- | O[log(n) + d*log(n)] where d is the degree of the matched node.
  merge (Context c) (Gr g) =
      let g1 = Map.insert c.node (Context' { incomers: fromAdj c.incomers
                                           , label: c.label
                                           , outgoers: fromAdj c.outgoers
                                           }
                                 ) g
          g2 = g1 # addSucc >>> addPred
       in Gr g2
    where addSucc graph = foldr (addEdge addIncomer) graph c.outgoers
          addPred graph = foldr (addEdge addOutgoer) graph c.incomers
          addEdge add (IncidentEdge { node, label }) =
            add { toAdd: { label, node: c.node }
                , addTo: node
                }

  updateNode f key (Gr g) = Gr $ Map.update (\c -> Just (updateContext c)) key g
    where updateContext (Context' c) = Context' c { label = f c.label }

  mapNodesWithKey :: forall k a b a'. (k -> a -> a') -> Gr k a b -> Gr k a' b
  mapNodesWithKey f (Gr g) = Gr $ mapWithIndex f' g
    where f' k (Context' c) = Context' (c { label = f k c.label })

  updateEdge f (Edge e) (Gr g) = Gr $ update g
    where updateSourceContext (Context' c) =
            Just $
            Context' c { outgoers = Map.update (Just <<< map f) e.to c.outgoers
                       }
          updateTargetContext (Context' c) =
            Just $
            Context' c { incomers = Map.update (Just <<< map f) e.from c.incomers
                       }
          update = Map.update updateSourceContext e.from >>> Map.update updateTargetContext e.to


type RmParams k = { toRemove :: k
                  , removeFrom :: k
                  }

rmOutgoer :: forall k a b. Ord k => RmParams k -> GrRep k a b -> GrRep k a b
rmOutgoer {toRemove,removeFrom} g = Map.update rmOutgoer' removeFrom g
  where rmOutgoer' (Context' c) = Just $ Context' $ c { outgoers = Map.delete toRemove c.outgoers }

rmIncomer :: forall k a b. Ord k => RmParams k -> GrRep k a b -> GrRep k a b
rmIncomer {toRemove,removeFrom} g = Map.update rmIncomer' removeFrom g
  where rmIncomer' (Context' c) = Just $ Context' $ c { incomers = Map.delete toRemove c.incomers }

type AddParams k b = { toAdd :: { label :: b
                                , node :: k
                                }
                     , addTo :: k
                     }

addIncomer :: forall k a b. Ord k => AddParams k b -> GrRep k a b -> GrRep k a b
addIncomer {toAdd,addTo} g = Map.update doAdd addTo g
  where doAdd (Context' c) = Just $ Context' $ c { incomers = Map.alter alter toAdd.node c.incomers }
        alter = case _ of
          Nothing -> Just $ List.singleton toAdd.label
          Just es -> Just $ toAdd.label `Cons` es 

addOutgoer :: forall k a b. Ord k => AddParams k b -> GrRep k a b -> GrRep k a b
addOutgoer {toAdd,addTo} g = Map.update doAdd addTo g
  where doAdd (Context' c) = Just $ Context' $ c { outgoers = Map.alter alter toAdd.node c.outgoers }
        alter = case _ of
          Nothing -> Just $ List.singleton toAdd.label
          Just es -> Just $ toAdd.label `Cons` es

edgeContextImpl :: forall k a b. Ord k => Edge k -> Gr k a b -> Maybe (EdgeContext k a b)
edgeContextImpl (Edge edge) (Gr graphRep) = do
  Context' source <- Map.lookup edge.from graphRep
  Context' target <- Map.lookup edge.to graphRep
  edgeLabels <- Map.lookup edge.to source.outgoers
  Just $ EdgeContext { edgeLabels
                     , source: LNode { node: edge.from, label: source.label }
                     , target: LNode { node: edge.to, label: target.label }
                     }

instance ordGraphGr :: OrdGraph Gr where
  minNode (Gr g) = _.key <$> Map.findMin g
  maxNode (Gr g) = _.key <$> Map.findMax g
