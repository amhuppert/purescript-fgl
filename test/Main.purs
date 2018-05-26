module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Array as Array
import Data.Graph.Inductive.Core (Context, Node, elem, empty, equal, isEmpty, labEdges, labNodes, match, mkGraph, mkUnlabeledGraph, nodeRange, order)
import Data.Graph.Inductive.Impl.Map (Gr)
import Data.Graph.Inductive.Query.DFS (dfs, revDfs)
import Data.Graph.Inductive.Query.TransitiveReduction (transitiveReduction)
import Data.List as List
import Data.Maybe (Maybe(Just, Nothing), isJust)
import Data.Tuple (Tuple(..), fst, snd)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  dfsTests
  transitiveReductionTests

  describe "Data.Map implementation of Graph and DynGraph classes" do
    describe "isEmpty" do
      it "should return true when given empty" do
        let g = empty :: Gr Unit Unit
        shouldEqual true (isEmpty g)
      pure unit
    describe "match" do
      it "loops should be included in the outgoers, but not in the incomers" do
        let g = mkGraph [ Tuple 1 unit ] [ Tuple {from: 1, to: 1} unit ] :: Gr Unit Unit
        shouldEqual 1 (order g)
        case match 1 g of
          Nothing -> fail "was nothing"
          Just { context, remaining } -> do
            Array.length context.outgoers `shouldEqual` 1
            Array.length context.incomers `shouldEqual` 0
      it "should match the correct node" do
        let g = mkGraph [ Tuple 1 "A"
                             , Tuple 2 "B"
                             ]
                             [ Tuple {from: 1, to: 2} unit
                             , Tuple {from: 1, to: 2} unit
                             , Tuple {from: 2, to: 1} unit
                             ] :: Gr String Unit
        case match 1 g of
          Just {context} -> do
            context.node `shouldEqual` 1
            context.label `shouldEqual` "A"
          Nothing -> fail $ "match returned Nothing"
      it "order of remaining graph should be one less" do
        let g = mkGraph [ Tuple 1 "A"
                        , Tuple 2 "B"
                        ]
                        [ Tuple {from: 1, to: 2} unit
                        , Tuple {from: 1, to: 2} unit
                        , Tuple {from: 2, to: 1} unit
                        ] :: Gr String Unit
        case match 1 g of
          Just {remaining} -> do
            order remaining `shouldEqual` 1
          Nothing -> fail $ "match returned Nothing"
      it "remaining graph should not contain the matched node" do
        let g = mkGraph [ Tuple 1 "A"
                        , Tuple 2 "B"
                        ]
                        [ Tuple {from: 1, to: 2} unit
                        , Tuple {from: 1, to: 2} unit
                        , Tuple {from: 2, to: 1} unit
                        ] :: Gr String Unit
        case match 1 g of
          Just {remaining} -> do
            elem 1 remaining `shouldEqual` false
          Nothing -> fail $ "match returned Nothing"
      it "should remove the incoming edge of each successor" do
        let g = mkGraph [ Tuple 1 "A"
                        , Tuple 2 "B"
                        , Tuple 3 "C"
                        ]
                        [ Tuple {from: 1, to: 2} unit
                        , Tuple {from: 2, to: 1} unit
                        , Tuple {from: 3, to: 1} unit
                        ] :: Gr String Unit
        case match 1 g of
          Just {remaining} ->
            case match 2 remaining of
              Just {context} -> do
                (context `hasIncomerFrom` 1) `shouldEqual` false
              Nothing -> fail $ "match 2 returned Nothing"
          Nothing -> fail $ "match 1 returned Nothing"
      it "should remove the outgoing edge of each predecessor" do
        let g = mkGraph [ Tuple 1 "A"
                        , Tuple 2 "B"
                        , Tuple 3 "C"
                        ]
                        [ Tuple {from: 1, to: 2} unit
                        , Tuple {from: 2, to: 1} unit
                        , Tuple {from: 3, to: 1} unit
                        ] :: Gr String Unit
        case match 1 g of
          Just {remaining} ->
            case match 3 remaining of
              Just {context} -> do
                (context `hasOutgoerTo` 1) `shouldEqual` false
              Nothing -> fail $ "match 3 returned Nothing"
          Nothing -> fail $ "match 1 returned Nothing"
    describe "mkGraph" do
      it "should succeed when creating graph with single vertex and no edges" do
        let g = mkGraph [ Tuple 1 unit ] [] :: Gr Unit Unit
        shouldEqual 1 (order g)
        shouldEqual true (isJust $ match 1 g)
      it "should succeed when creating graph with single vertex and a simple loop" do
        let g = mkGraph [ Tuple 1 unit ] [ Tuple {from: 1, to: 1} unit ] :: Gr Unit Unit
        shouldEqual 1 (order g)
        case match 1 g of
          Nothing -> fail "was nothing"
          Just _ -> pure unit
      it "should succeed when creating graph with a valid (non-loop) edge" do
        let g = mkGraph [ Tuple 1 unit, Tuple 2 unit ] [ Tuple {from: 1, to: 2} unit ] :: Gr Unit Unit
        shouldEqual 2 (order g)
      it "should add multiple edges to the same vertex" do
        let g = mkGraph [ Tuple 1 unit, Tuple 2 unit ] [ Tuple {from: 1, to: 2} unit
                                                       , Tuple {from: 1, to: 2} unit ] :: Gr Unit Unit
        shouldEqual 2 (order g)
        shouldEqual 2 (Array.length $ labEdges g)
  describe "labNodes" do
      it "should give a list of all nodes" do
        let g = mkGraph [ Tuple 1 "A"
                        , Tuple 2 "B"
                        ]
                        [ Tuple {from: 1, to: 2} unit
                        , Tuple {from: 1, to: 2} unit
                        ] :: Gr String Unit
        let lns = Array.sortWith fst $ labNodes g
        case lns of
          [Tuple 1 "A", Tuple 2 "B"] -> pure unit
          r -> fail $ "labNodes returned: " <> show r
  describe "order" do
      it "should be 0 on empty" do
        let g = empty :: Gr Unit Unit
        (order g) `shouldEqual` 0
      it "should return the correct number of nodes" do
        let g = mkGraph [ Tuple 1 "A"
                        , Tuple 2 "B"
                        ]
                        [ Tuple {from: 1, to: 2} unit
                        , Tuple {from: 1, to: 2} unit
                        , Tuple {from: 2, to: 1} unit
                        ] :: Gr String Unit
        order g `shouldEqual` 2
  describe "nodeRange" do
      it "should work correctly" do
        let g = mkGraph [ Tuple 9 unit, Tuple 0 unit, Tuple 5 unit, Tuple (-2) unit
                        ]
                        [
                        ] :: Gr Unit Unit
        let mrange = nodeRange g
        case mrange of
          Nothing -> fail "nodeRange returned Nothing"
          Just {min,max} -> do
            min `shouldEqual` (-2)
            max `shouldEqual` 9
  describe "labEdges" do
      it "should give a list of all edges (including loops, duplicates, etc.)" do
        let g = mkGraph [ Tuple 1 unit
                        , Tuple 2 unit
                        ]
                        [ Tuple {from: 1, to: 2} "A"
                        , Tuple {from: 1, to: 2} "B"
                        , Tuple {from: 2, to: 1} "C"
                        , Tuple {from: 2, to: 2} "D"
                        ] :: Gr Unit String
        let lns = Array.sortWith snd $ labEdges g
        case lns of
          [Tuple {from: 1, to: 2} "A",Tuple {from: 1, to: 2} "B",Tuple {from: 2, to: 1} "C",Tuple {from: 2, to: 2} "D"] -> do
            pure unit
          r -> fail "labNodes returned bad result"

hasIncomerFrom :: forall a b. Context a b -> Node -> Boolean
hasIncomerFrom context from = (not <<< Array.null) $ Array.filter (snd >>> (_ == from)) context.incomers

hasOutgoerTo :: forall a b. Context a b -> Node -> Boolean
hasOutgoerTo context to = (not <<< Array.null) $ Array.filter (snd >>> (_ == to)) context.outgoers

transitiveReductionTests :: forall r. Spec r Unit
transitiveReductionTests = describe "Data.Graph.Inductive.Query.TransitiveReduction" do
    it "should reduce simple graph" do
      let result = transitiveReduction simple
          matchesExpected = result `equal` trOfSimple
      matchesExpected `shouldEqual` true

  where simple :: Gr Unit Unit
        simple = mkUnlabeledGraph
          [1,2,3]
          [ { from: 1, to: 2 }
          , { from: 2, to: 3 }
          , { from: 1, to: 3 }
          ]
        trOfSimple = mkUnlabeledGraph
          [1,2,3]
          [ { from: 1, to: 2 }
          , { from: 2, to: 3 }
          ]

dfsTests :: forall r. Spec r Unit
dfsTests = describe "Data.Graph.Inductive.Query.DFS" do
    it "dfs traverses 1->2->3->4 starting with 1 in correct order" do
      let result = Array.fromFoldable $ dfs (List.singleton 1) lineSegmentsGraph
      result `shouldEqual` [1,2,3,4]
    it "revDfs traverses 1->2->3->4 starting with 4 in reverse order" do
      let result = Array.fromFoldable $ revDfs (List.singleton 4) lineSegmentsGraph
      result `shouldEqual` [4,3,2,1]

  where lineSegmentsGraph :: Gr Unit Unit
        lineSegmentsGraph = mkUnlabeledGraph
              [1,2,3,4]
              [ {from: 1, to: 2}
              , {from: 2, to: 3}
              , {from: 3, to: 4}
              ]
