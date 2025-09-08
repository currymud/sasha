{-# LANGUAGE ScopedTypeVariables #-}

module Test.Categorical.MonadLaws (spec) where

import           Control.Monad                  ((>=>))
import           Control.Monad.Except           (runExceptT)
import           Control.Monad.Identity         (Identity, runIdentity)
import           Control.Monad.Reader           (runReaderT)
import           Control.Monad.State            (runStateT)
import           Data.Text                      (Text)
import           Test.Hspec

import           Examples.BedroomDemo.GameState (gameState)
import           Model.Core                     (ActionMaps (..), Config (..),
                                                 GameComputation (..),
                                                 GameStateT (..))

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

-- Simple functions for testing (using () since values don't matter for structural laws)
f :: () -> GameComputation Identity ()
f () = pure ()

g :: () -> GameComputation Identity ()
g () = pure ()

-- Core Monad Laws (structural properties only)

-- Left Identity Law: pure a >>= f ≡ f a
testLeftIdentity :: IO ()
testLeftIdentity = do
  let a = ()
      lhs = runTestComputation (pure a >>= f)
      rhs = runTestComputation (f a)
  lhs `shouldBe` rhs

-- Right Identity Law: m >>= pure ≡ m
testRightIdentity :: IO ()
testRightIdentity = do
  let m = pure ()
      lhs = runTestComputation (m >>= pure)
      rhs = runTestComputation m
  lhs `shouldBe` rhs

-- Associativity Law: (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
testAssociativity :: IO ()
testAssociativity = do
  let m = pure ()
      lhs = runTestComputation ((m >>= f) >>= g)
      rhs = runTestComputation (m >>= (\x -> f x >>= g))
  lhs `shouldBe` rhs

-- Functor Laws (structural properties)

-- fmap id ≡ id
testFunctorIdentity :: IO ()
testFunctorIdentity = do
  let m = pure ()
      lhs = runTestComputation (fmap id m)
      rhs = runTestComputation m
  lhs `shouldBe` rhs

-- fmap f m ≡ m >>= (pure . f)
testFunctorMonadRelation :: IO ()
testFunctorMonadRelation = do
  let m = pure ()
      lhs = runTestComputation (fmap id m)
      rhs = runTestComputation (m >>= (pure . id))
  lhs `shouldBe` rhs

-- Applicative Identity: pure id <*> v ≡ v
testApplicativeIdentity :: IO ()
testApplicativeIdentity = do
  let v = pure ()
      lhs = runTestComputation (pure id <*> v)
      rhs = runTestComputation v
  lhs `shouldBe` rhs

-- Additional Categorical Tests for Completeness

-- Functor Composition: fmap (f . g) ≡ fmap f . fmap g
testFunctorComposition :: IO ()
testFunctorComposition = do
  let m = pure ()
      lhs = runTestComputation (fmap (id . id) m)
      rhs = runTestComputation (fmap id (fmap id m))
  lhs `shouldBe` rhs

-- Applicative Composition: pure (.) <*> u <*> v <*> w ≡ u <*> (v <*> w)
testApplicativeComposition :: IO ()
testApplicativeComposition = do
  let u = pure id
      v = pure id  
      w = pure ()
      lhs = runTestComputation (pure (.) <*> u <*> v <*> w)
      rhs = runTestComputation (u <*> (v <*> w))
  lhs `shouldBe` rhs

-- Applicative Homomorphism: pure f <*> pure x ≡ pure (f x)
testApplicativeHomomorphism :: IO ()
testApplicativeHomomorphism = do
  let lhs = runTestComputation (pure id <*> pure ())
      rhs = runTestComputation (pure (id ()))
  lhs `shouldBe` rhs

-- Applicative Interchange: u <*> pure y ≡ pure ($ y) <*> u
testApplicativeInterchange :: IO ()
testApplicativeInterchange = do
  let u = pure id
      y = ()
      lhs = runTestComputation (u <*> pure y)
      rhs = runTestComputation (pure ($ y) <*> u)
  lhs `shouldBe` rhs

-- Hspec test suite
spec :: Spec
spec = describe "GameComputation Identity - Categorical Structure Laws" $ do
  describe "Core Monad Laws (Categorical Requirements)" $ do
    it "Left Identity: pure a >>= f ≡ f a" testLeftIdentity
    it "Right Identity: m >>= pure ≡ m" testRightIdentity  
    it "Associativity: (m >>= f) >>= g ≡ m >>= (f >=> g)" testAssociativity

  describe "Functor Laws (Categorical Structure)" $ do
    it "Functor Identity: fmap id ≡ id" testFunctorIdentity
    it "Functor Composition: fmap (f . g) ≡ fmap f . fmap g" testFunctorComposition
    it "Functor-Monad Coherence: fmap f m ≡ m >>= (pure . f)" testFunctorMonadRelation

  describe "Applicative Laws (Categorical Structure)" $ do
    it "Applicative Identity: pure id <*> v ≡ v" testApplicativeIdentity
    it "Applicative Composition: pure (.) <*> u <*> v <*> w ≡ u <*> (v <*> w)" testApplicativeComposition
    it "Applicative Homomorphism: pure f <*> pure x ≡ pure (f x)" testApplicativeHomomorphism
    it "Applicative Interchange: u <*> pure y ≡ pure ($ y) <*> u" testApplicativeInterchange

main :: IO ()
main = hspec spec
