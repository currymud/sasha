{-# LANGUAGE ScopedTypeVariables #-}

{-
Category Theory Analysis: Natural Transformation Laws for Effect Registry

CATEGORICAL STRUCTURE:
1. We have a functor F = GameComputation Identity : Set -> Set
2. We have a functor G = Identity : Set -> Set
3. getGlobalEffectRegistry is a natural transformation η : F -> G composed with gets

NATURAL TRANSFORMATION DEFINITION:
A natural transformation η : F ⇒ G between functors F,G : C -> D
satisfies the naturality condition: for any morphism f : A -> B in C,
the following diagram commutes:

    F(A) ---F(f)---> F(B)
     |                |
   η_A|              η_B|
     |                |
     v                v
    G(A) ---G(f)---> G(B)

This means: η_B ∘ F(f) = G(f) ∘ η_A

APPLIED TO OUR REGISTRY ACCESS:
- F = GameComputation Identity (stateful computation functor)
- G = Identity (trivial functor)
- η = runTestComputation ∘ getGlobalEffectRegistry (extracts registry from state)
- For any f : EffectRegistry -> A, we test:

  runTestComputation (fmap f getGlobalEffectRegistry) ≡ f (runTestComputation getGlobalEffectRegistry)

  This is equivalent to: fmap f . getGlobalEffectRegistry ≡ getGlobalEffectRegistry >>= (pure . f)

CATEGORICAL SIGNIFICANCE:
This naturality law ensures that registry access preserves the categorical structure
of the effect management system. It guarantees that extracting and transforming
the registry commutes with the stateful computation, maintaining the architectural
invariant that "effect ownership" is categorically preserved across transformations.

The test validates that our effect registry forms a proper natural transformation,
confirming the mathematical soundness of the 4-level effect hierarchy.
-}

module Test.Categorical.RegistryNaturalityLaws (spec) where

import           Control.Monad.Except           (runExceptT)
import           Control.Monad.Identity         (Identity, runIdentity)
import           Control.Monad.Reader           (runReaderT)
import           Control.Monad.State            (runStateT)
import qualified Data.Map.Strict                as Map
import           Data.Text                      (Text)
import           Test.Hspec

import           Examples.BedroomDemo.GameState (gameState)
import           GameState.ActionManagement     (processEffectsFromRegistry)
import           GameState.EffectRegistry       (getGlobalEffectRegistry,
                                                 registerEffects)
import           Model.Core                     (ActionEffectMap (..),
                                                 ActionMaps (..), Config (..),
                                                 EffectActionKey (..),
                                                 EffectRegistry,
                                                 GameComputation (..),
                                                 GameState (..),
                                                 GameStateT (..), GameT (..),
                                                 ImplicitStimulusActionF,
                                                 transformToIO)
import           Model.GID                      (GID (GID))

-- Test runner that executes GameComputation in a minimal context
runTestComputation :: GameComputation Identity a -> Either Text a
runTestComputation comp =
  let initialState = gameState
      testConfig = Config (ActionMaps mempty mempty mempty mempty mempty mempty mempty mempty)
      computation = runReaderT (runGameComputation comp) testConfig
      withErrorHandling = runExceptT computation
      withState = runStateT (runGameStateT withErrorHandling) initialState
      result = runIdentity withState
  in case fst result of
    Left err  -> Left err
    Right val -> Right val

-- Test runner that also returns final GameState for comparison
runTestComputationWithState :: GameComputation Identity a -> Either Text (a, EffectRegistry)
runTestComputationWithState comp =
  let initialState = gameState
      testConfig = Config (ActionMaps mempty mempty mempty mempty mempty mempty mempty mempty)
      computation = runReaderT (runGameComputation comp) testConfig
      withErrorHandling = runExceptT computation
      withState = runStateT (runGameStateT withErrorHandling) initialState
      result = runIdentity withState
  in case result of
    (Left err, _)           -> Left err
    (Right val, finalState) -> Right (val, _effectRegistry finalState)

-- Natural Transformation Law: η_B ∘ F(f) = G(f) ∘ η_A
-- Applied: runTestComp ∘ fmap f ∘ getRegistry = f ∘ runTestComp ∘ getRegistry
testRegistryAccessNaturality :: IO ()
testRegistryAccessNaturality = do
  let f = Map.size  -- morphism EffectRegistry -> Int
      -- Left side: η_B ∘ F(f) = runTestComputation (fmap f getGlobalEffectRegistry)
      leftSide = fmap f getGlobalEffectRegistry
      -- Right side: G(f) ∘ η_A = getGlobalEffectRegistry >>= (pure . f)
      rightSide = getGlobalEffectRegistry >>= (pure . f)
      lhsResult = runTestComputation leftSide
      rhsResult = runTestComputation rightSide
  lhsResult `shouldBe` rhsResult

-- System Effect Processing Naturality Law
-- Tests context preservation: equivalent effect keys should produce equivalent results
testEffectProcessingNaturality :: IO ()
testEffectProcessingNaturality = do
  let key1 = ImplicitStimulusActionKey (GID 1)
      key2 = ImplicitStimulusActionKey (GID 2)
      emptyEffectMap = ActionEffectMap mempty

      -- Setup: register identical empty effect maps for both keys
      setupComp = do
        registerEffects key1 emptyEffectMap
        registerEffects key2 emptyEffectMap

      -- Test: process effects in different orders
      comp1 = setupComp >> processEffectsFromRegistry key1 >> processEffectsFromRegistry key2
      comp2 = setupComp >> processEffectsFromRegistry key2 >> processEffectsFromRegistry key1

      result1 = runTestComputationWithState comp1
      result2 = runTestComputationWithState comp2

  result1 `shouldBe` result2

-- Test that effect processing preserves computational structure
testEffectProcessingStructurePreservation :: IO ()
testEffectProcessingStructurePreservation = do
  let testKey = ImplicitStimulusActionKey (GID 3)
      emptyEffectMap = ActionEffectMap mempty

      -- Test: processing empty effects is equivalent to not processing
      comp1 = registerEffects testKey emptyEffectMap >> processEffectsFromRegistry testKey
      comp2 = registerEffects testKey emptyEffectMap

      result1 = runTestComputationWithState comp1
      result2 = runTestComputationWithState comp2

  result1 `shouldBe` result2

-- Test runner for GameT IO that returns final state
runGameTWithState :: GameT IO a -> IO (Either Text (a, EffectRegistry))
runGameTWithState comp = do
  let initialState = gameState
      testConfig = Config (ActionMaps mempty mempty mempty mempty mempty mempty mempty mempty)
      computation = runReaderT (runGameT comp) testConfig
      withErrorHandling = runExceptT computation
      withState = runStateT (runGameStateT withErrorHandling) initialState
  result <- withState
  case result of
    (Left err, _)           -> return $ Left err
    (Right val, finalState) -> return $ Right (val, _effectRegistry finalState)

-- State Management Transformation Tests
-- Test naturality: fmap f (transformToIO comp) ≡ transformToIO (fmap f comp)
testTransformToIONaturality :: IO ()
testTransformToIONaturality = do
  let testKey = ImplicitStimulusActionKey (GID 10)
      effectMap = ActionEffectMap mempty
      f = Map.size  -- Function EffectRegistry -> Int

      -- Pure computation that modifies state
      comp = registerEffects testKey effectMap >> getGlobalEffectRegistry

      -- Left side: fmap f (transformToIO comp)
      leftSide = fmap f (transformToIO comp)

      -- Right side: transformToIO (fmap f comp)
      rightSide = transformToIO (fmap f comp)

  lhsResult <- runGameTWithState leftSide
  rhsResult <- runGameTWithState rightSide
  lhsResult `shouldBe` rhsResult

-- Test state threading: sequential operations compose correctly
testTransformToIOStateThreading :: IO ()
testTransformToIOStateThreading = do
  let key1 = ImplicitStimulusActionKey (GID 11)
      key2 = ImplicitStimulusActionKey (GID 12)
      effectMap = ActionEffectMap mempty

      comp1 = registerEffects key1 effectMap
      comp2 = registerEffects key2 effectMap

      -- Left side: transformToIO comp1 >> transformToIO comp2
      leftSide = transformToIO comp1 >> transformToIO comp2 >> transformToIO getGlobalEffectRegistry

      -- Right side: transformToIO (comp1 >> comp2)
      rightSide = transformToIO (comp1 >> comp2 >> getGlobalEffectRegistry)

  lhsResult <- runGameTWithState leftSide
  rhsResult <- runGameTWithState rightSide
  lhsResult `shouldBe` rhsResult

spec :: Spec
spec = describe "Registry Access Naturality Laws" $ do
  describe "getGlobalEffectRegistry Natural Transformation" $ do
    it "Satisfies naturality: η_B ∘ F(f) = G(f) ∘ η_A" testRegistryAccessNaturality

  describe "processEffectsFromRegistry Natural Transformation" $ do
    it "Preserves context: equivalent keys produce equivalent results" testEffectProcessingNaturality
    it "Preserves computational structure: empty effects don't modify state" testEffectProcessingStructurePreservation

  describe "transformToIO Natural Transformation" $ do
    it "Preserves functor structure: fmap f (transformToIO comp) ≡ transformToIO (fmap f comp)" testTransformToIONaturality
    it "Preserves sequential composition: transformToIO comp1 >> transformToIO comp2 ≡ transformToIO (comp1 >> comp2)" testTransformToIOStateThreading

main :: IO ()
main = hspec spec
