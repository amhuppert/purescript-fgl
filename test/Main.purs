module Test.Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console as Console
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Graph.Inductive.Core (Context, Node, elem, empty, isEmpty, labEdges, labNodes, match, mkGraph, nodeRange, order)
import Data.Graph.Inductive.Impl.Map (Gr)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Tuple (Tuple(..), fst, snd)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "Data.Map implementation of Graph and DynGraph classes" do
    describe "isEmpty" do
      it "should return true when given empty" do
        let g = empty :: Gr Unit Unit
        shouldEqual true (isEmpty g)
      pure unit
    describe "match" do
      it "loops should be included in the outgoers, but not in the incomers" do
        let result = mkGraph [ Tuple 1 unit ] [ Tuple {from: 1, to: 1} unit ] :: Either String (Gr Unit Unit)
        withRight result \g -> do
          shouldEqual 1 (order g)
          case match 1 g of
            Nothing -> fail "was nothing"
            Just { context, remaining } -> do
              Array.length context.outgoers `shouldEqual` 1
              Array.length context.incomers `shouldEqual` 0
      it "should match the correct node" do
        let result = mkGraph [ Tuple 1 "A"
                             , Tuple 2 "B"
                             ]
                             [ Tuple {from: 1, to: 2} unit
                             , Tuple {from: 1, to: 2} unit
                             , Tuple {from: 2, to: 1} unit
                             ] :: Either String (Gr String Unit)
        withRight result \g ->
          case match 1 g of
            Just {context} -> do
              context.node `shouldEqual` 1
              context.label `shouldEqual` "A"
            Nothing -> fail $ "match returned Nothing"
      it "order of remaining graph should be one less" do
        let result = mkGraph [ Tuple 1 "A"
                             , Tuple 2 "B"
                             ]
                             [ Tuple {from: 1, to: 2} unit
                             , Tuple {from: 1, to: 2} unit
                             , Tuple {from: 2, to: 1} unit
                             ] :: Either String (Gr String Unit)
        withRight result \g ->
          case match 1 g of
            Just {remaining} -> do
              order remaining `shouldEqual` 1
            Nothing -> fail $ "match returned Nothing"
      it "remaining graph should not contain the matched node" do
        let result = mkGraph [ Tuple 1 "A"
                             , Tuple 2 "B"
                             ]
                             [ Tuple {from: 1, to: 2} unit
                             , Tuple {from: 1, to: 2} unit
                             , Tuple {from: 2, to: 1} unit
                             ] :: Either String (Gr String Unit)
        withRight result \g ->
          case match 1 g of
            Just {remaining} -> do
              elem 1 remaining `shouldEqual` false
            Nothing -> fail $ "match returned Nothing"
      it "should remove the incoming edge of each successor" do
        let result = mkGraph [ Tuple 1 "A"
                             , Tuple 2 "B"
                             , Tuple 3 "C"
                             ]
                             [ Tuple {from: 1, to: 2} unit
                             , Tuple {from: 2, to: 1} unit
                             , Tuple {from: 3, to: 1} unit
                             ] :: Either String (Gr String Unit)
        withRight result \g ->
          case match 1 g of
            Just {remaining} ->
              case match 2 remaining of
                Just {context} -> do
                  (context `hasIncomerFrom` 1) `shouldEqual` false
                Nothing -> fail $ "match 2 returned Nothing"
            Nothing -> fail $ "match 1 returned Nothing"
      it "should remove the outgoing edge of each predecessor" do
        let result = mkGraph [ Tuple 1 "A"
                             , Tuple 2 "B"
                             , Tuple 3 "C"
                             ]
                             [ Tuple {from: 1, to: 2} unit
                             , Tuple {from: 2, to: 1} unit
                             , Tuple {from: 3, to: 1} unit
                             ] :: Either String (Gr String Unit)
        withRight result \g ->
          case match 1 g of
            Just {remaining} ->
              case match 3 remaining of
                Just {context} -> do
                  (context `hasOutgoerTo` 1) `shouldEqual` false
                Nothing -> fail $ "match 3 returned Nothing"
            Nothing -> fail $ "match 1 returned Nothing"
    describe "mkGraph" do
      it "should succeed for Nil input" do
        let result = mkGraph [] [] :: Either String (Gr Unit Unit)
        assertRight result
      it "should succeed when creating graph with single vertex and no edges" do
        let result = mkGraph [ Tuple 1 unit ] [] :: Either String (Gr Unit Unit)
        withRight result \g -> do
          shouldEqual 1 (order g)
          shouldEqual true (isJust $ match 1 g)
      it "should succeed when creating graph with single vertex and a simple loop" do
        let result = mkGraph [ Tuple 1 unit ] [ Tuple {from: 1, to: 1} unit ] :: Either String (Gr Unit Unit)
        withRight result \g -> do
          shouldEqual 1 (order g)
          case match 1 g of
            Nothing -> fail "was nothing"
            Just _ -> pure unit
      it "should succeed when creating graph with a valid (non-loop) edge" do
        let result = mkGraph [ Tuple 1 unit, Tuple 2 unit ] [ Tuple {from: 1, to: 2} unit ] :: Either String (Gr Unit Unit)
        withRight result \g -> do
          shouldEqual 2 (order g)
      it "should add multiple edges to the same vertex" do
        let result = mkGraph [ Tuple 1 unit, Tuple 2 unit ] [ Tuple {from: 1, to: 2} unit
                                                            , Tuple {from: 1, to: 2} unit ] :: Either String (Gr Unit Unit)
        withRight result \g -> do
          shouldEqual 2 (order g)
          shouldEqual 2 (Array.length $ labEdges g)
    describe "labNodes" do
      it "should give a list of all nodes" do
        let result = mkGraph [ Tuple 1 "A"
                             , Tuple 2 "B"
                             ]
                             [ Tuple {from: 1, to: 2} unit
                             , Tuple {from: 1, to: 2} unit
                             ] :: Either String (Gr String Unit)
        withRight result \g ->
          let lns = Array.sortWith fst $ labNodes g
           in case lns of
                [Tuple 1 "A", Tuple 2 "B"] -> pure unit
                r -> fail $ "labNodes returned: " <> show r
    describe "order" do
      it "should be 0 on empty" do
        let g = empty :: Gr Unit Unit
        (order g) `shouldEqual` 0
      it "should return the correct number of nodes" do
        let result = mkGraph [ Tuple 1 "A"
                             , Tuple 2 "B"
                             ]
                             [ Tuple {from: 1, to: 2} unit
                             , Tuple {from: 1, to: 2} unit
                             , Tuple {from: 2, to: 1} unit
                             ] :: Either String (Gr String Unit)
        withRight result \g ->
          order g `shouldEqual` 2
    describe "nodeRange" do
      it "should work correctly" do
        let result = mkGraph [ Tuple 9 unit, Tuple 0 unit, Tuple 5 unit, Tuple (-2) unit
                             ]
                             [
                             ] :: Either String (Gr Unit Unit)
        withRight result \g -> do
          let mrange = nodeRange g
          case mrange of
            Nothing -> fail "nodeRange returned Nothing"
            Just {min,max} -> do
              min `shouldEqual` (-2)
              max `shouldEqual` 9
    describe "labEdges" do
      it "should give a list of all edges (including loops, duplicates, etc.)" do
        let result = mkGraph [ Tuple 1 unit
                             , Tuple 2 unit
                             ]
                             [ Tuple {from: 1, to: 2} "A"
                             , Tuple {from: 1, to: 2} "B"
                             , Tuple {from: 2, to: 1} "C"
                             , Tuple {from: 2, to: 2} "D"
                             ] :: Either String (Gr Unit String)
        withRight result \g ->
          let lns = Array.sortWith snd $ labEdges g
           in case lns of
                [Tuple {from: 1, to: 2} "A",Tuple {from: 1, to: 2} "B",Tuple {from: 2, to: 1} "C",Tuple {from: 2, to: 2} "D"] -> do
                  pure unit
                r -> fail "labNodes returned bad result"

assertRight :: forall a b eff. Either a b -> Aff eff Unit
assertRight = case _ of
  Right _ -> pure unit
  Left _ -> fail "was left"

withRight :: forall a b eff. Show a => Either a b -> (b -> Aff eff Unit) -> Aff eff Unit
withRight val onRight = case val of
  Right x -> onRight x
  Left e -> fail $ "was Left: " <> show e

hasIncomerFrom :: forall a b. Context a b -> Node -> Boolean
hasIncomerFrom context from = (not <<< Array.null) $ Array.filter (snd >>> (_ == from)) context.incomers

hasOutgoerTo :: forall a b. Context a b -> Node -> Boolean
hasOutgoerTo context to = (not <<< Array.null) $ Array.filter (snd >>> (_ == to)) context.outgoers
