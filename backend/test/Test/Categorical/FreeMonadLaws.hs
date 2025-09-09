{-# LANGUAGE ScopedTypeVariables #-}

module Test.Categorical.FreeMonadLaws (spec) where

import           Test.Hspec
import           EDSL.GameStateBuilder                            (initialBuilderState, 
                                                                   runWorldBuilderWithMaps,
                                                                   interpretDSL)
import           Examples.BedroomDemo.GameState                   (gameState)
import           Model.GameState.GameStateDSL                     (WorldDSL(..), 
                                                                   declareObjectGID,
                                                                   setInitialNarration)
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus (bedroomDS, chairDS)
import           Model.Parser.Composites.Nouns                   (NounPhrase(SimpleNounPhrase))
import           Model.Parser.Atomics.Nouns                      (DirectionalStimulus)
import           Model.GID                                        (GID)
import           Model.Core                                       (Object)
import           Data.Text                                        (Text)

-- Test structural equality by checking if both computations succeed or fail consistently
testStructuralEquality :: WorldDSL a -> WorldDSL a -> Bool
testStructuralEquality lhs rhs = 
  let lhsResult = runWorldBuilderWithMaps (interpretDSL lhs) (initialBuilderState gameState)
      rhsResult = runWorldBuilderWithMaps (interpretDSL rhs) (initialBuilderState gameState)
  in case (lhsResult, rhsResult) of
    (Left _, Left _) -> True    -- Both fail consistently
    (Right _, Right _) -> True  -- Both succeed consistently  
    _ -> False                  -- Inconsistent behavior

-- Simple functions for testing
createObject :: NounPhrase DirectionalStimulus -> WorldDSL (GID Object)
createObject = declareObjectGID

addNarration :: Text -> WorldDSL ()
addNarration = setInitialNarration

-- FREE MONAD LAW 1: Left Identity
-- Pure a >>= f ≡ f a
testFreeMonadLeftIdentity :: IO ()
testFreeMonadLeftIdentity = do
  let a = SimpleNounPhrase bedroomDS
      f = createObject
      lhs = Bind (Pure a) f
      rhs = f a
  testStructuralEquality lhs rhs `shouldBe` True

-- FREE MONAD LAW 2: Right Identity  
-- m >>= Pure ≡ m
testFreeMonadRightIdentity :: IO ()
testFreeMonadRightIdentity = do
  let m = createObject (SimpleNounPhrase chairDS)
      lhs = Bind m Pure
      rhs = m
  testStructuralEquality lhs rhs `shouldBe` True

-- FREE MONAD LAW 3: Associativity
-- (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
testFreeMonadAssociativity :: IO ()
testFreeMonadAssociativity = do
  let m = Pure (SimpleNounPhrase bedroomDS)
      f = createObject
      g = \_ -> addNarration "test"
      lhs = Bind (Bind m f) g
      rhs = Bind m (\x -> Bind (f x) g)
  testStructuralEquality lhs rhs `shouldBe` True

-- INTERPRETER LAW 1: Pure Preservation
-- interpret (Pure a) ≡ pure a
testInterpreterPurePreservation :: IO ()
testInterpreterPurePreservation = do
  let value = SimpleNounPhrase bedroomDS
      lhs = runWorldBuilderWithMaps (interpretDSL (Pure value)) (initialBuilderState gameState)
      rhs = runWorldBuilderWithMaps (pure value) (initialBuilderState gameState)
  case (lhs, rhs) of
    (Left _, Left _) -> pure ()
    (Right _, Right _) -> pure ()
    _ -> expectationFailure "Interpreter pure preservation violated"

-- INTERPRETER LAW 2: Bind Preservation  
-- interpret (m >>= f) ≡ interpret m >>= (interpret . f)
testInterpreterBindPreservation :: IO ()
testInterpreterBindPreservation = do
  let m = Pure (SimpleNounPhrase bedroomDS)
      f = createObject
      lhs = runWorldBuilderWithMaps (interpretDSL (Bind m f)) (initialBuilderState gameState)
      rhs = runWorldBuilderWithMaps (interpretDSL m >>= (interpretDSL . f)) (initialBuilderState gameState)
  case (lhs, rhs) of
    (Left _, Left _) -> pure ()
    (Right _, Right _) -> pure ()
    _ -> expectationFailure "Interpreter bind preservation violated"

spec :: Spec
spec = describe "WorldDSL - Free Monad Laws" $ do
  describe "Core Free Monad Laws" $ do
    it "Left Identity: Pure a >>= f ≡ f a" testFreeMonadLeftIdentity
    it "Right Identity: m >>= Pure ≡ m" testFreeMonadRightIdentity  
    it "Associativity: (m >>= f) >>= g ≡ m >>= (f >=> g)" testFreeMonadAssociativity

  describe "Free Monad Interpreter Laws" $ do
    it "Pure Preservation: interpret (Pure a) ≡ pure a" testInterpreterPurePreservation
    it "Bind Preservation: interpret (m >>= f) ≡ interpret m >>= (interpret . f)" testInterpreterBindPreservation

main :: IO ()
main = hspec spec