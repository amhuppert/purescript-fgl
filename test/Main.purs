module Test.Main where

import Prelude

import Data.Array as Array
import Data.Function (on)
import Data.Lazy as Lazy
import Data.List (List(..))
import Data.List as List
import Data.List.Lazy as LL
import Data.List.Lazy as LazyList
import Data.Maybe (Maybe(Just, Nothing), isJust)
import Effect (Effect)
import Graph.Inductive.Algorithms.DFS (dfs, revDfs)
import Graph.Inductive.Algorithms.TransitiveReduction (transitiveReduction)
import Graph.Inductive.Class (empty, isEmpty, labEdges, labNodes, match, maxNode, minNode, mkGraph, order)
import Graph.Inductive.Class as Graph
import Graph.Inductive.Core (labelEdge, labelNode, mkUnlabeledGraph)
import Graph.Inductive.Core as Core
import Graph.Inductive.Equality (equal)
import Graph.Inductive.Impl (Gr)
import Graph.Inductive.Inspect (elem)
import Graph.Inductive.Tree as Tree
import Graph.Inductive.Types (Context(..), Edge(..), GraphDecomposition(..), LEdge(..), LNode(..))
import Graph.Inductive.Types.Accessors as A
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main :: Effect Unit
main = run [consoleReporter] $ do
  dfsTests
  transitiveReductionTests
  coreTests
  treeTests
  describe "Data.Map implementation of Graph and DynGraph classes" do
    describe "isEmpty" do
      it "should return true when given empty" do
        let g = empty :: Gr Int Unit Unit
        shouldEqual true (isEmpty g)
      pure unit
    describe "match" do
      it "should match the correct node" do
        let g = mkGraph [ labelNode 1 "A"
                        , labelNode 2 "B"
                        ]
                        [ labelEdge (Edge {from: 1, to: 2}) unit
                        , labelEdge (Edge {from: 1, to: 2}) unit
                        , labelEdge (Edge {from: 2, to: 1}) unit
                        ] :: Gr Int String Unit
        case match 1 g of
          Just (Decomp {context: Context context}) -> do
            context.node `shouldEqual` 1
            context.label `shouldEqual` "A"
          Nothing -> fail $ "match returned Nothing"
      it "order of remaining graph should be one less" do
        let g = mkGraph [ labelNode 1 "A"
                        , labelNode 2 "B"
                        ]
                        [ labelEdge (Edge {from: 1, to: 2}) unit
                        , labelEdge (Edge {from: 1, to: 2}) unit
                        , labelEdge (Edge {from: 2, to: 1}) unit
                        ] :: Gr Int String Unit
        case match 1 g of
          Just (Decomp {remaining}) -> do
            order (Lazy.force remaining) `shouldEqual` 1
          Nothing -> fail $ "match returned Nothing"
      it "remaining graph should not contain the matched node" do
        let g = mkGraph [ labelNode 1 "A"
                        , labelNode 2 "B"
                        ]
                        [ labelEdge (Edge {from: 1, to: 2}) unit
                        , labelEdge (Edge {from: 1, to: 2}) unit
                        , labelEdge (Edge {from: 2, to: 1}) unit
                        ] :: Gr Int String Unit
        case match 1 g of
          Just (Decomp {remaining}) -> do
            elem 1 (Lazy.force remaining) `shouldEqual` false
          Nothing -> fail $ "match returned Nothing"
      it "should remove the incoming edge of each successor" do
        let g = mkGraph [ labelNode 1 "A"
                        , labelNode 2 "B"
                        , labelNode 3 "C"
                        ]
                        [ labelEdge (Edge {from: 1, to: 2}) unit
                        , labelEdge (Edge {from: 2, to: 1}) unit
                        , labelEdge (Edge {from: 3, to: 1}) unit
                        ] :: Gr Int String Unit
        case match 1 g of
          Just (Decomp {remaining}) ->
            case match 2 (Lazy.force remaining) of
              Just (Decomp {context}) -> do
                (context `hasIncomerFrom` 1) `shouldEqual` false
              Nothing -> fail $ "match 2 returned Nothing"
          Nothing -> fail $ "match 1 returned Nothing"
      it "should remove the outgoing edge of each predecessor" do
        let g = mkGraph [ labelNode 1 "A"
                        , labelNode 2 "B"
                        , labelNode 3 "C"
                        ]
                        [ labelEdge (Edge {from: 1, to: 2}) unit
                        , labelEdge (Edge {from: 2, to: 1}) unit
                        , labelEdge (Edge {from: 3, to: 1}) unit
                        ] :: Gr Int String Unit
        case match 1 g of
          Just (Decomp {remaining}) ->
            case match 3 (Lazy.force remaining) of
              Just (Decomp {context}) -> do
                (context `hasOutgoerTo` 1) `shouldEqual` false
              Nothing -> fail $ "match 3 returned Nothing"
          Nothing -> fail $ "match 1 returned Nothing"
    describe "mkGraph" do
      it "should succeed when creating graph with single vertex and no edges" do
        let g = mkGraph [ labelNode 1 unit ] [] :: Gr Int Unit Unit
        shouldEqual 1 (order g)
        shouldEqual true (isJust $ match 1 g)
      it "should succeed when creating graph with single vertex and a simple loop" do
        let g = mkGraph [ labelNode 1 unit ] [ labelEdge (Edge {from: 1, to: 1}) unit ] :: Gr Int Unit Unit
        shouldEqual 1 (order g)
        case match 1 g of
          Nothing -> fail "was nothing"
          Just _ -> pure unit
      it "should succeed when creating graph with a valid (non-loop) edge" do
        let g = mkGraph [ labelNode 1 unit, labelNode 2 unit ] [ labelEdge (Edge {from: 1, to: 2}) unit ] :: Gr Int Unit Unit
        shouldEqual 2 (order g)
      it "should add multiple edges to the same vertex" do
        let g = mkGraph [ labelNode 1 unit, labelNode 2 unit ] [ labelEdge (Edge {from: 1, to: 2}) unit
                                                       , labelEdge (Edge {from: 1, to: 2}) unit ] :: Gr Int Unit Unit
        shouldEqual 2 (order g)
        shouldEqual 2 (List.length $ labEdges g)
  describe "labNodes" $ do
      it "should give a list of all nodes" $ do
        let g = mkGraph [ labelNode 1 "A"
                        , labelNode 2 "B"
                        ]
                        [ labelEdge (Edge {from: 1, to: 2}) unit
                        , labelEdge (Edge {from: 1, to: 2}) unit
                        ] :: Gr Int String Unit
        let lns = List.sortBy (compare `on` A.nodeFromLNode) $ labNodes g
        case lns of
          Cons (LNode { node: 1, label: "A"}) (Cons (LNode { node: 2, label: "B" }) Nil) -> pure unit
          r -> fail $ "labNodes returned: " <> show r
  describe "order" do
      it "should be 0 on empty" do
        let g = empty :: Gr Int Unit Unit
        (order g) `shouldEqual` 0
      it "should return the correct number of nodes" do
        let g = mkGraph [ labelNode 1 "A"
                        , labelNode 2 "B"
                        ]
                        [ labelEdge (Edge {from: 1, to: 2}) unit
                        , labelEdge (Edge {from: 1, to: 2}) unit
                        , labelEdge (Edge {from: 2, to: 1}) unit
                        ] :: Gr Int String Unit
        order g `shouldEqual` 2
  describe "nodeRange" do
      it "should work correctly" do
        let g = mkGraph [ labelNode 9 unit, labelNode 0 unit, labelNode 5 unit, labelNode (-2) unit
                        ]
                        [
                        ] :: Gr Int Unit Unit
        let mrange = do
              min <- minNode g
              max <- maxNode g
              pure { min, max }
        case mrange of
          Nothing -> fail "nodeRange returned Nothing"
          Just {min,max} -> do
            min `shouldEqual` (-2)
            max `shouldEqual` 9
  describe "labEdges" do
      it "should give a list of all edges (including loops, duplicates, etc.)" do
        let g = mkGraph [ labelNode 1 unit
                        , labelNode 2 unit
                        ]
                        [ labelEdge (Edge {from: 1, to: 2}) "A"
                        , labelEdge (Edge {from: 1, to: 2}) "B"
                        , labelEdge (Edge {from: 2, to: 1}) "C"
                        , labelEdge (Edge {from: 2, to: 2}) "D"
                        ] :: Gr Int Unit String
        let lns = Array.fromFoldable $ List.sortBy (compare `on` A.labelFromLEdge) $ labEdges g
        case lns of
          [ LEdge { edge: Edge {from: 1, to: 2}, label: "A" },
            LEdge { edge: Edge {from: 1, to: 2}, label: "B" },
            LEdge { edge: Edge {from: 2, to: 1}, label: "C" },
            LEdge { edge: Edge {from: 2, to: 2}, label: "D" }] -> do
              pure unit
          r -> fail "labNodes returned bad result"

hasIncomerFrom :: forall k a b. Eq k => Context k a b -> k -> Boolean
hasIncomerFrom (Context context) from = (not <<< List.null) $ List.filter (A.nodeFromIncidentEdge >>> (_ == from)) context.incomers

hasOutgoerTo :: forall k a b. Eq k => Context k a b -> k -> Boolean
hasOutgoerTo (Context context) to = (not <<< List.null) $ List.filter (A.nodeFromIncidentEdge >>> (_ == to)) context.outgoers

transitiveReductionTests :: Spec Unit
transitiveReductionTests = describe "Graph.Inductive.Algorithms.TransitiveReduction" do
    it "should reduce simple graph" do
      let result = transitiveReduction simple
          matchesExpected = result `equal` trOfSimple
      matchesExpected `shouldEqual` true
    it "should reduce another simple graph g2" do
      let result = transitiveReduction g2
          matchesExpected = result `equal` trOfG2
      matchesExpected `shouldEqual` true
    it "should reduce another simple graph g3" do
      let result = transitiveReduction g3
          matchesExpected = result `equal` trOfG3
      matchesExpected `shouldEqual` true

  where simple :: Gr Int Unit Unit
        simple = mkUnlabeledGraph
          [1,2,3]
          [ Edge { from: 1, to: 2 }
          , Edge { from: 2, to: 3 }
          , Edge { from: 1, to: 3 }
          ]
        trOfSimple = mkUnlabeledGraph
          [1,2,3]
          [ Edge { from: 1, to: 2 }
          , Edge { from: 2, to: 3 }
          ]
        g2 :: Gr Char Unit Unit
        g2 = mkUnlabeledGraph
          [ 'A', 'B', 'C', 'D' ]
          [ Edge { from: 'C', to: 'B' }
          , Edge { from: 'B', to: 'D' }
          , Edge { from: 'A', to: 'B' }
          , Edge { from: 'A', to: 'D' } -- this edge should be removed
          ]
        trOfG2 :: Gr Char Unit Unit
        trOfG2 = mkUnlabeledGraph
          [ 'A', 'B', 'C', 'D' ]
          [ Edge { from: 'C', to: 'B' }
          , Edge { from: 'B', to: 'D' }
          , Edge { from: 'A', to: 'B' }
          ]
        g3 :: Gr Char Unit Unit
        g3 = mkUnlabeledGraph
          [ 'A', 'B', 'C', 'D' ]
          [ Edge { from: 'C', to: 'B' }
          , Edge { from: 'B', to: 'A' }
          , Edge { from: 'D', to: 'B' }
          , Edge { from: 'D', to: 'A' } -- this edge should be removed
          ]
        trOfG3 :: Gr Char Unit Unit
        trOfG3 = mkUnlabeledGraph
          [ 'A', 'B', 'C', 'D' ]
          [ Edge { from: 'C', to: 'B' }
          , Edge { from: 'B', to: 'A' }
          , Edge { from: 'D', to: 'B' }
          ]

dfsTests :: Spec Unit
dfsTests = describe "Graph.Inductive.Algorithms.DFS" do
    it "dfs traverses 1->2->3->4 starting with 1 in correct order" do
      let result = Array.fromFoldable $ dfs (LazyList.singleton 1) lineSegmentsGraph
      result `shouldEqual` [1,2,3,4]
    it "revDfs traverses 1->2->3->4 starting with 4 in reverse order" do
      let result = Array.fromFoldable $ revDfs (LazyList.singleton 4) lineSegmentsGraph
      result `shouldEqual` [4,3,2,1]

  where lineSegmentsGraph :: Gr Int Unit Unit
        lineSegmentsGraph = mkUnlabeledGraph
              [1,2,3,4]
              [ Edge {from: 1, to: 2}
              , Edge {from: 2, to: 3}
              , Edge {from: 3, to: 4}
              ]


coreTests :: Spec Unit
coreTests = describe "Graph.Inductive.Core" $ do
  describe "newNodes" $ do
    it "should produce a list whose length matches the count parameter" $ do
      let graph = Graph.empty :: Gr Int Unit Unit
          count = 5
          res = Core.newNodes count graph
      LL.length res `shouldEqual` count
    it "should produce unique results" $ do
      let graph = Graph.empty :: Gr Int Unit Unit
          count = 5
          res = Core.newNodes count graph
      LL.length (LL.nub res) `shouldEqual` count

treeTests :: Spec Unit
treeTests =
  describe "Graph.Inductive.Tree" do
    it "preorder" do
      let results = Tree.preorder tree
      results `shouldEqual` LazyList.fromFoldable ['A', 'B', 'D', 'E', 'C', 'F', 'G']
    it "postorder" do
      let results = Tree.postorder tree
      results `shouldEqual` LazyList.fromFoldable ['D', 'E', 'B', 'F', 'G', 'C', 'A']

  where tree =
          Tree.mkTree 'A'
            [ Tree.mkTree 'B'
                [ Tree.mkTree 'D' []
                , Tree.mkTree 'E' []
                ]
            , Tree.mkTree 'C'
                [ Tree.mkTree 'F' []
                , Tree.mkTree 'G' []
                ]
            ]
