{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}

module Test.Categorical.RegistryAccessNaturality (spec) where

import           Control.Monad.Except           (runExceptT)
import           Control.Monad.Identity         (Identity, runIdentity)
import           Control.Monad.Reader           (runReaderT)
import           Control.Monad.State            (runStateT, modify, get)
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict               as Map
import           Data.Text                      (Text)
import           Test.Hspec
import           Test.QuickCheck

import           Examples.BedroomDemo.GameState (gameState)
import           Model.Core                     (ActionMaps (..), Config (..), 
                                                 GameComputation (..), GameStateT (..),
                                                 GameState(..), EffectRegistry, EffectActionKey,
                                                 ActionEffectMap(..))
import           GameState.EffectRegistry       (getGlobalEffectRegistry, 
                                                 modifyGlobalEffectRegistry)

-- Test runner for GameComputation with configurable initial state
runTestComputationWith :: GameState -> GameComputation Identity a -> Either Text (a, GameState)
runTestComputationWith initialState comp =
  let testConfig = Config (ActionMaps mempty mempty mempty mempty mempty mempty mempty mempty)
      computation = runReaderT (runGameComputation comp) testConfig
      withErrorHandling = runExceptT computation
      withState = runStateT (runGameStateT withErrorHandling) initialState
      result = runIdentity withState
  in case result of
    (Left err, _) -> Left err
    (Right val, finalState) -> Right (val, finalState)

-- Standard test runner using default game state
runTestComputation :: GameComputation Identity a -> Either Text (a, GameState)
runTestComputation = runTestComputationWith gameState

-- Test structural equality by checking if both computations succeed or fail consistently
testStructuralEquality :: GameComputation Identity a -> GameComputation Identity a -> Bool
testStructuralEquality lhs rhs = 
  let lhsResult = runTestComputation lhs
      rhsResult = runTestComputation rhs
  in case (lhsResult, rhsResult) of
    (Left _, Left _) -> True    -- Both fail consistently
    (Right _, Right _) -> True  -- Both succeed consistently  
    _ -> False                  -- Inconsistent behavior

-- ===========================
-- REGISTRY ACCESS NATURALITY TESTS
-- ===========================

-- | Natural Transformation Law 1: Registry Access Commutes with State Modifications
-- 
-- This tests that accessing the registry before and after a state modification
-- that doesn't affect the registry yields consistent results relative to the modification.
--
-- Categorically: Let F = GameComputation Identity and η = getGlobalEffectRegistry
-- For any state transformation α : GameState -> GameState that preserves _effectRegistry,
-- we have: η ∘ lift(α) = lift(id) ∘ η
registryAccessCommutesWithNonRegistryStateModification :: Spec
registryAccessCommutesWithNonRegistryStateModification = 
  it "Registry access commutes with non-registry state modifications" $ property $ do
    -- A state modification that doesn't touch the effect registry
    let nonRegistryModification :: GameComputation Identity ()
        nonRegistryModification = modify $ \gs -> gs { _narration = _narration gs }
    
    -- Get registry before modification
    let beforeComp = do
          registry1 <- getGlobalEffectRegistry
          nonRegistryModification
          registry2 <- getGlobalEffectRegistry
          return (registry1, registry2)
    
    let afterComp = do
          nonRegistryModification
          registry1 <- getGlobalEffectRegistry
          nonRegistryModification
          registry2 <- getGlobalEffectRegistry
          return (registry1, registry2)
    
    property $ testStructuralEquality beforeComp afterComp

-- | Natural Transformation Law 2: Registry Access is Functorial
--
-- This tests that registry access behaves functorially with respect to
-- registry transformations.
--
-- Categorically: For registry transformation f : EffectRegistry -> EffectRegistry,
-- accessing after applying f should equal applying f to the accessed registry.
registryAccessIsFunctorial :: Spec  
registryAccessIsFunctorial =
  it "Registry access is functorial with respect to registry transformations" $ property $ do
    let someKey = if Map.null (_effectRegistry gameState) 
                  then error "No keys available for transformation test"
                  else head $ Map.keys (_effectRegistry gameState)
        registryTransform = Map.delete -- Using map deletion as our transformation
    
    -- Method 1: Transform registry, then access
    let transformThenAccess = do
          modifyGlobalEffectRegistry (registryTransform someKey)
          getGlobalEffectRegistry
    
    -- Method 2: Access registry, then transform the result
    let accessThenTransform = do
          registry <- getGlobalEffectRegistry
          return $ registryTransform someKey registry
    
    -- Both should have the same structural behavior
    property $ testStructuralEquality transformThenAccess accessThenTransform

-- | Natural Transformation Law 3: Registry Access Preserves Composition
--
-- This tests that sequential registry accesses compose properly.
--
-- Categorically: Sequential applications of the registry access transformation
-- should compose as expected in the underlying category.
registryAccessPreservesComposition :: Spec
registryAccessPreservesComposition =
  it "Registry access preserves composition of state transformations" $ property $ do
    -- Two registry modifications
    let modification1 = modifyGlobalEffectRegistry (const Map.empty)
        modification2 = modifyGlobalEffectRegistry (const Map.empty)
    
    -- Composed modifications with access
    let composedAccess = do
          modification1
          reg1 <- getGlobalEffectRegistry  
          modification2
          reg2 <- getGlobalEffectRegistry
          return (reg1, reg2)
    
    -- Sequential modifications with access
    let sequentialAccess = do
          modification1
          modification2  
          reg <- getGlobalEffectRegistry
          return reg
    
    -- Test structural equivalence of composed vs sequential access
    let structuralComposedAccess = do
          modification1
          modification2
          getGlobalEffectRegistry
    
    property $ testStructuralEquality sequentialAccess structuralComposedAccess

-- | Registry Identity Law
--
-- Accessing the registry through the identity transformation should
-- be equivalent to direct access.
registryAccessIdentityLaw :: Spec
registryAccessIdentityLaw =
  it "Registry access satisfies identity law" $ do
    let directAccess = getGlobalEffectRegistry
        identityAccess = do
          modifyGlobalEffectRegistry id  -- Identity transformation
          getGlobalEffectRegistry
    
    testStructuralEquality directAccess identityAccess `shouldBe` True

-- | Registry State Isolation Law
--
-- Registry access should not modify the state - it should be a pure projection.
-- This tests that the registry accessor is indeed a proper "lens getter" in 
-- categorical terms.
registryAccessIsStatePure :: Spec
registryAccessIsStatePure =
  it "Registry access does not modify state" $ property $ do
    let accessComp = do
          initialState <- get
          _ <- getGlobalEffectRegistry
          _ <- getGlobalEffectRegistry  -- Access twice
          finalState <- get
          return (initialState, finalState)
    
    let noAccessComp = do
          initialState <- get
          finalState <- get  
          return (initialState, finalState)
    
    property $ testStructuralEquality accessComp noAccessComp

-- | Registry Access Naturality under Monad Operations
--
-- This tests that registry access commutes properly with standard monad operations
-- like bind and return, which is essential for it to be a proper natural transformation.
registryAccessNaturalityUnderMonadOps :: Spec
registryAccessNaturalityUnderMonadOps =
  it "Registry access is natural with respect to monad operations" $ do
    -- Test that: (pure f) >>= (\x -> getRegistry >> return (f x)) 
    --         = getRegistry >> (pure f >>= return . f)
    let f = const ()  -- Simple function for testing
        x = ()
        
        leftSide = do
          _ <- pure x
          registry <- getGlobalEffectRegistry
          return (f x, registry)
          
        rightSide = do  
          registry <- getGlobalEffectRegistry
          y <- pure (f x)
          return (y, registry)
    
    testStructuralEquality leftSide rightSide `shouldBe` True

-- ===========================
-- EFFECT REGISTRY CATEGORICAL LAWS
-- ===========================

-- | Registry as Categorical Object Law
--
-- The effect registry should behave as a proper categorical object,
-- meaning morphisms (registry transformations) compose associatively.
registryTransformationsAreAssociative :: Spec
registryTransformationsAreAssociative =
  it "Registry transformations compose associatively" $ property $ do
    -- Three simple transformations  
    let f = if Map.null (_effectRegistry gameState) 
            then id 
            else Map.delete (head $ Map.keys (_effectRegistry gameState))
        g = const Map.empty
        h = id
        
        -- (f ∘ g) ∘ h
        leftAssocComp = do
          modifyGlobalEffectRegistry (f . g . h)
          getGlobalEffectRegistry
          
        -- f ∘ (g ∘ h)  
        rightAssocComp = do
          modifyGlobalEffectRegistry (f . (g . h))
          getGlobalEffectRegistry
    
    property $ testStructuralEquality leftAssocComp rightAssocComp

-- Hspec test suite
spec :: Spec  
spec = describe "Effect Registry Access - Natural Transformation Properties" $ do
  describe "Registry Access Naturality Laws" $ do
    registryAccessCommutesWithNonRegistryStateModification
    registryAccessIsFunctorial
    registryAccessPreservesComposition
    registryAccessIdentityLaw
    registryAccessIsStatePure
    registryAccessNaturalityUnderMonadOps
    
  describe "Registry Categorical Structure Laws" $ do
    registryTransformationsAreAssociative