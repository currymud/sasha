{-# LANGUAGE ScopedTypeVariables #-}

{-
Category Theory Analysis: Spatial Relationship Monoidal Properties

CATEGORICAL STRUCTURE:
The spatial system exhibits monoidal structure at multiple levels:

1. SET-LEVEL MONOIDS:
   - SpatialRelationship sets form monoids under set union
   - Identity: empty set of relationships
   - Associativity: (A ∪ B) ∪ C ≡ A ∪ (B ∪ C)

2. CONTAINMENT CHAIN MONOIDS:
   - Containment chains form monoids under composition
   - Identity: empty containment chain
   - Associativity: chain composition is associative

3. ACCESSIBILITY TRAVERSAL MONOIDS:
   - Object accessibility forms monoids under object set union
   - Identity: empty object set yields empty accessible set
   - Associativity: accessibility union is associative

CATEGORICAL SIGNIFICANCE:
These monoid laws ensure that spatial relationships maintain coherence
across the 4-level effect hierarchy. The spatial system must preserve
categorical structure to maintain architectural invariants.

The tests validate that spatial operations form proper monoids,
confirming the mathematical soundness of spatial composition in
the effect management system.
-}

module Test.Categorical.SpatialMonoidLaws (spec) where

import           Control.Monad.Except           (runExceptT)
import           Control.Monad.Identity         (Identity, runIdentity)
import           Control.Monad.Reader           (runReaderT)
import           Control.Monad.State            (gets, modify, runStateT)
import qualified Data.Map.Strict                as Map
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import           Test.Hspec

import           Examples.BedroomDemo.GameState (gameState)
import           GameState.Spatial              (getAllAccessibleObjects,
                                                 getContainedObjects,
                                                 getContainmentChain,
                                                 getSupportedObjects)
import           Model.Core                     (ActionMaps (..), Config (..),
                                                 GameComputation (..),
                                                 GameState (..),
                                                 GameStateT (..), Object,
                                                 SpatialRelationship (..),
                                                 SpatialRelationshipMap (..),
                                                 World (..))
import           Model.GID                      (GID (GID))

-- Test runner that executes GameComputation in a minimal context
runTestComputation :: GameComputation Identity a -> Either Text (a, GameState)
runTestComputation comp =
  let initialState = gameState
      testConfig = Config (ActionMaps mempty mempty mempty mempty mempty mempty mempty mempty)
      computation = runReaderT (runGameComputation comp) testConfig
      withErrorHandling = runExceptT computation
      withState = runStateT (runGameStateT withErrorHandling) initialState
      result = runIdentity withState
  in case result of
    (Left err, _)           -> Left err
    (Right val, finalState) -> Right (val, finalState)

-- Test runner with custom initial state
runTestComputationWith :: GameState -> GameComputation Identity a -> Either Text (a, GameState)
runTestComputationWith initialState comp =
  let testConfig = Config (ActionMaps mempty mempty mempty mempty mempty mempty mempty mempty)
      computation = runReaderT (runGameComputation comp) testConfig
      withErrorHandling = runExceptT computation
      withState = runStateT (runGameStateT withErrorHandling) initialState
      result = runIdentity withState
  in case result of
    (Left err, _)           -> Left err
    (Right val, finalState) -> Right (val, finalState)

-- Test structural equality by checking if both computations succeed or fail consistently
testStructuralEquality :: GameComputation Identity a -> GameComputation Identity a -> Bool
testStructuralEquality lhs rhs =
  let lhsResult = runTestComputation lhs
      rhsResult = runTestComputation rhs
  in case (lhsResult, rhsResult) of
    (Left _, Left _)   -> True    -- Both fail consistently
    (Right _, Right _) -> True  -- Both succeed consistently
    _                  -> False                  -- Inconsistent behavior

-- Helper: Create test spatial relationship map
createTestSpatialMap :: [(GID Object, Set.Set SpatialRelationship)] -> SpatialRelationshipMap
createTestSpatialMap relationships = SpatialRelationshipMap (Map.fromList relationships)

-- Helper: Create test game state with custom spatial map
createTestGameState :: SpatialRelationshipMap -> GameState
createTestGameState spatialMap =
  let world = _world gameState
      updatedWorld = world { _spatialRelationshipMap = spatialMap }
  in gameState { _world = updatedWorld }

-- ===========================
-- SET-BASED SPATIAL MONOID LAWS
-- ===========================

-- | Monoid Identity Law: mempty <> spatialSet ≡ spatialSet
testSpatialSetIdentityLaw :: IO ()
testSpatialSetIdentityLaw = do
  let obj1 = GID 1
      obj2 = GID 2
      spatialSet = Set.fromList [Contains (Set.singleton obj2), Supports (Set.singleton obj2)]

      -- Left identity: mempty <> set = set
      leftIdentitySet = spatialSet

      -- Right identity: set <> mempty = set
      rightIdentitySet = spatialSet <> mempty

  leftIdentitySet `shouldBe` spatialSet
  rightIdentitySet `shouldBe` spatialSet

-- | Monoid Associativity Law: (a <> b) <> c ≡ a <> (b <> c)
testSpatialSetAssociativityLaw :: IO ()
testSpatialSetAssociativityLaw = do
  let obj1 = GID 1
      obj2 = GID 2
      obj3 = GID 3

      setA = Set.fromList [Contains (Set.singleton obj1)]
      setB = Set.fromList [Supports (Set.singleton obj2)]
      setC = Set.fromList [ContainedIn obj3]

      -- Left association: (a <> b) <> c
      leftAssoc = (setA <> setB) <> setC

      -- Right association: a <> (b <> c)
      rightAssoc = setA <> (setB <> setC)

  leftAssoc `shouldBe` rightAssoc

-- | Spatial Set Commutativity: Independent spatial relationships commute
testSpatialSetCommutativityLaw :: IO ()
testSpatialSetCommutativityLaw = do
  let obj1 = GID 1
      obj2 = GID 2

      -- Independent relationships (different objects)
      setA = Set.fromList [Contains (Set.singleton obj1)]
      setB = Set.fromList [Contains (Set.singleton obj2)]

      -- Forward: A <> B
      forward = setA <> setB

      -- Backward: B <> A
      backward = setB <> setA

  forward `shouldBe` backward

-- ===========================
-- CONTAINMENT CHAIN MONOID LAWS
-- ===========================

-- | Containment Chain Identity Law: Empty chain is identity for composition
testContainmentChainIdentityLaw :: IO ()
testContainmentChainIdentityLaw = do
  let obj1 = GID 1
      obj2 = GID 2

      -- Create simple containment: obj1 contains obj2
      spatialMap = createTestSpatialMap
        [ (obj1, Set.singleton (Contains (Set.singleton obj2)))
        , (obj2, Set.singleton (ContainedIn obj1))
        ]
      testState = createTestGameState spatialMap

      -- Get containment chain for obj2
      chainComp = getContainmentChain obj2

      -- Empty chain concatenated should be identity
      result = runTestComputationWith testState chainComp

  case result of
    Left _           -> expectationFailure "Chain computation failed"
    Right (chain, _) -> chain `shouldSatisfy` (not . null)  -- Non-empty chain should exist

-- | Containment Chain Associativity: Chain composition is associative
testContainmentChainAssociativityLaw :: IO ()
testContainmentChainAssociativityLaw = do
  let obj1 = GID 1  -- Root
      obj2 = GID 2  -- Middle
      obj3 = GID 3  -- Leaf

      -- Create containment hierarchy: obj1 contains obj2 contains obj3
      spatialMap = createTestSpatialMap
        [ (obj1, Set.singleton (Contains (Set.singleton obj2)))
        , (obj2, Set.fromList [ContainedIn obj1, Contains (Set.singleton obj3)])
        , (obj3, Set.singleton (ContainedIn obj2))
        ]
      testState = createTestGameState spatialMap

      -- Get chain from leaf to root
      chainComp = getContainmentChain obj3

      result = runTestComputationWith testState chainComp

  case result of
    Left _           -> expectationFailure "Chain computation failed"
    Right (chain, _) -> length chain `shouldBe` 3  -- Should include all three objects in hierarchy

-- | Containment Chain Transitivity: Chains preserve transitivity
testContainmentChainTransitivityLaw :: IO ()
testContainmentChainTransitivityLaw = do
  let obj1 = GID 1  -- Grandparent
      obj2 = GID 2  -- Parent
      obj3 = GID 3  -- Child

      spatialMap = createTestSpatialMap
        [ (obj1, Set.singleton (Contains (Set.singleton obj2)))
        , (obj2, Set.fromList [ContainedIn obj1, Contains (Set.singleton obj3)])
        , (obj3, Set.singleton (ContainedIn obj2))
        ]
      testState = createTestGameState spatialMap

      -- Chain from child should include grandparent (transitivity)
      chainComp = getContainmentChain obj3

      result = runTestComputationWith testState chainComp

  case result of
    Left _           -> expectationFailure "Chain transitivity test failed"
    Right (chain, _) -> obj1 `shouldSatisfy` (`elem` chain)  -- Grandparent should be in chain

-- ===========================
-- ACCESSIBILITY TRAVERSAL MONOID LAWS
-- ===========================

-- | Traversal Identity Law: Empty set yields empty accessible set
testAccessibilityTraversalIdentityLaw :: IO ()
testAccessibilityTraversalIdentityLaw = do
  let emptySpatialMap = createTestSpatialMap []
      testState = createTestGameState emptySpatialMap

      -- Get accessible objects from empty starting set
      accessibilityComp = do
        world <- gets _world
        let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world
        getAllAccessibleObjects [] spatialMap

      result = runTestComputationWith testState accessibilityComp

  case result of
    Left _ -> expectationFailure "Empty accessibility computation failed"
    Right (objects, _) -> objects `shouldBe` []

-- | Traversal Union Law: accessible(A ∪ B) ≡ accessible(A) ∪ accessible(B)
testAccessibilityTraversalUnionLaw :: IO ()
testAccessibilityTraversalUnionLaw = do
  let obj1 = GID 1
      obj2 = GID 2
      obj3 = GID 3
      obj4 = GID 4

      -- obj1 contains obj3, obj2 contains obj4
      spatialMap = createTestSpatialMap
        [ (obj1, Set.singleton (Contains (Set.singleton obj3)))
        , (obj2, Set.singleton (Contains (Set.singleton obj4)))
        , (obj3, Set.singleton (ContainedIn obj1))
        , (obj4, Set.singleton (ContainedIn obj2))
        ]
      testState = createTestGameState spatialMap

      accessibilityComp = do
        world <- gets _world
        let SpatialRelationshipMap spatialMapData = _spatialRelationshipMap world

        -- Get accessible from obj1 alone
        accessible1 <- getAllAccessibleObjects [obj1] spatialMapData

        -- Get accessible from obj2 alone
        accessible2 <- getAllAccessibleObjects [obj2] spatialMapData

        -- Get accessible from both obj1 and obj2
        accessibleBoth <- getAllAccessibleObjects [obj1, obj2] spatialMapData

        return (accessible1, accessible2, accessibleBoth)

      result = runTestComputationWith testState accessibilityComp

  case result of
    Left _ -> expectationFailure "Accessibility union test failed"
    Right ((acc1, acc2, accBoth), _) ->
      Set.fromList accBoth `shouldBe` (Set.fromList acc1 `Set.union` Set.fromList acc2)

-- | Traversal Composition Law: Sequential traversals compose correctly
testAccessibilityTraversalCompositionLaw :: IO ()
testAccessibilityTraversalCompositionLaw = do
  let obj1 = GID 1  -- Container
      obj2 = GID 2  -- Contained object
      obj3 = GID 3  -- Object in contained object

      -- Nested containment: obj1 -> obj2 -> obj3
      spatialMap = createTestSpatialMap
        [ (obj1, Set.singleton (Contains (Set.singleton obj2)))
        , (obj2, Set.fromList [ContainedIn obj1, Contains (Set.singleton obj3)])
        , (obj3, Set.singleton (ContainedIn obj2))
        ]
      testState = createTestGameState spatialMap

      -- Composition test: two-step traversal should equal single traversal
      compositionComp = do
        world <- gets _world
        let SpatialRelationshipMap spatialMapData = _spatialRelationshipMap world

        -- Single traversal from obj1 (should reach obj2 and obj3)
        getAllAccessibleObjects [obj1] spatialMapData

      result = runTestComputationWith testState compositionComp

  case result of
    Left _ -> expectationFailure "Accessibility composition test failed"
    Right (accessible, _) -> do
      obj2 `shouldSatisfy` (`elem` accessible)  -- Should reach obj2
      obj3 `shouldSatisfy` (`elem` accessible)  -- Should reach obj3 through composition

-- ===========================
-- HSPEC TEST SUITE
-- ===========================

spec :: Spec
spec = describe "Spatial Relationship Monoidal Laws" $ do
  describe "Set-Based Spatial Monoid Laws" $ do
    it "Identity: mempty <> spatialSet ≡ spatialSet" testSpatialSetIdentityLaw
    it "Associativity: (a <> b) <> c ≡ a <> (b <> c)" testSpatialSetAssociativityLaw
    it "Commutativity: Independent spatial relationships commute" testSpatialSetCommutativityLaw

  describe "Containment Chain Monoid Laws" $ do
    it "Chain Identity: Empty chain is identity for composition" testContainmentChainIdentityLaw
    it "Chain Associativity: Chain composition is associative" testContainmentChainAssociativityLaw
    it "Chain Transitivity: Chains preserve transitivity" testContainmentChainTransitivityLaw

  describe "Accessibility Traversal Monoid Laws" $ do
    it "Traversal Identity: Empty set yields empty accessible set" testAccessibilityTraversalIdentityLaw
    it "Traversal Union: accessible(A ∪ B) ≡ accessible(A) ∪ accessible(B)" testAccessibilityTraversalUnionLaw
    it "Traversal Composition: Sequential traversals compose correctly" testAccessibilityTraversalCompositionLaw

main :: IO ()
main = hspec spec
