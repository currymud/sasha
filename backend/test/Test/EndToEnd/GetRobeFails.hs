{-
End-to-End Test - Phase 1: Setup & Initial Failure
Tests that "get robe" fails initially with expected message
-}

module Test.EndToEnd.GetRobeFails (spec) where

import qualified Data.Set                       as Set
import           Test.Hspec

import           Examples.BedroomDemo.GameState (gameState)
import           Model.Core                     (_actionConsequence, _inventory,
                                                 _narration, _player)
import           Test.TestRunner                (executeCommand)

-- Phase 1: Test that "get robe" fails initially
testInitialGetRobeFailure :: IO ()
testInitialGetRobeFailure = do
  result <- executeCommand "get robe" gameState
  case result of
    Left err -> expectationFailure $ "Command execution failed: " <> show err
    Right finalState -> do
      let narration = _narration finalState
          consequences = _actionConsequence narration
          player = _player finalState
          inventory = _inventory player

      -- Should get failure message from getDeniedF
      consequences `shouldContain` ["You try but feel dizzy and have to lay back down"]

      -- Inventory should remain empty
      inventory `shouldBe` Set.empty

-- Test that initial game state is set up correctly
testInitialGameStateSetup :: IO ()
testInitialGameStateSetup = do
  let player = _player gameState
      inventory = _inventory player

  -- Player should start with empty inventory
  inventory `shouldBe` Set.empty

spec :: Spec
spec = describe "Phase 1: Setup & Initial Failure" $ do
  it "Game state initializes with empty inventory" testInitialGameStateSetup
  it "Get robe fails with expected denial message" testInitialGetRobeFailure

main :: IO ()
main = hspec spec
