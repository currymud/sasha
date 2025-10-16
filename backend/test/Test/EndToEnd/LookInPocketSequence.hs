{-# LANGUAGE OverloadedStrings #-}

{-
End-to-End Test - Look In Pocket Sequence
Tests the complete sequence: "open eyes" → "get robe" → "look in pocket"
Expected final result: "The pocket is closed."
-}

module Test.EndToEnd.LookInPocketSequence (spec) where

import qualified Data.List           (isInfixOf, singleton)
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Test.Hspec

import           Examples.Initialize (gameState)
import           Model.Core          (GameState, _actionConsequence, _inventory,
                                      _narration, _objectMap, _player,
                                      _shortName, _world)
import           Model.Core.Mappings (_getGIDToDataMap)
import           Test.TestRunner     (executeCommand)

-- Helper function to check if robe is in inventory
hasRobeInInventory :: GameState -> Bool
hasRobeInInventory gs =
  let inventory = _inventory (_player gs)
      objectMap = (_getGIDToDataMap . _objectMap) (_world gs)
  in any (\objGID ->
    case Map.lookup objGID objectMap of
      Just obj -> "<OBJ-003>" `T.isInfixOf` _shortName obj
      Nothing  -> False) inventory

-- Test the exact sequence from REPL: open eyes → get robe → look in pocket
testLookInPocketSequence :: IO ()
testLookInPocketSequence = do
  -- Step 1: "open eyes"
  eyesResult <- executeCommand "open eyes" gameState
  case eyesResult of
    Left err -> expectationFailure $ "Open eyes failed: " <> show err
    Right eyesState -> do
      let eyesConsequences = _actionConsequence (_narration eyesState)
      eyesConsequences `shouldContain` [T.pack "You open your eyes, and the world comes into focus."]

      -- Step 2: "get robe"
      robeResult <- executeCommand "get robe" eyesState
      case robeResult of
        Left err -> expectationFailure $ "Get robe failed: " <> show err
        Right robeState -> do
          let robeConsequences = _actionConsequence (_narration robeState)
          robeConsequences `shouldContain` [T.pack "You pick it up."]

          -- Step 3: "look in pocket"
          pocketResult <- executeCommand "look in pocket" robeState
          case pocketResult of
            Left err -> expectationFailure $ "Look in pocket failed: " <> show err
            Right finalState -> do
              let finalConsequences = _actionConsequence (_narration finalState)
              finalConsequences `shouldContain` [T.pack "The pocket is closed."]

-- Test that initial "look in pocket" works (pocket is accessible)
testInitialLookInPocket :: IO ()
testInitialLookInPocket = do
  result <- executeCommand "look in pocket" gameState
  case result of
    Left err -> expectationFailure $ "Look in pocket should work: " <> show err
    Right state -> do
      let consequences = _actionConsequence (_narration state)
      consequences `shouldSatisfy` (== ["The pocket is closed."])

-- Test individual steps work in isolation
testOpenEyesWorks :: IO ()
testOpenEyesWorks = do
  result <- executeCommand "open eyes" gameState
  case result of
    Left err -> expectationFailure $ "Open eyes failed: " <> show err
    Right state -> do
      let consequences = _actionConsequence (_narration state)
      consequences `shouldSatisfy` (any (T.isInfixOf "You open your eyes, and the world comes into focus."))

-- Test that after opening eyes and getting robe, we can see the perception changes
testPerceptionProgression :: IO ()
testPerceptionProgression = do
  -- Execute "open eyes" first
  eyesResult <- executeCommand "open eyes" gameState
  case eyesResult of
    Left err -> expectationFailure $ "Open eyes failed: " <> show err
    Right eyesState -> do
      -- Execute "get robe"
      robeResult <- executeCommand "get robe" eyesState
      case robeResult of
        Left err -> expectationFailure $ "Get robe failed: " <> show err
        Right robeState -> do
          hasRobeInInventory robeState `shouldBe` True

          -- Now the pocket should be accessible for looking into
          pocketResult <- executeCommand "look in pocket" robeState
          case pocketResult of
            Left err -> expectationFailure $ "Look in pocket should work after getting robe: " <> show err
            Right finalState -> do
              let consequences = _actionConsequence (_narration finalState)
              -- Should get "The pocket is closed." message
              consequences `shouldSatisfy` (== ["The pocket is closed."])

spec :: Spec
spec = describe "Look In Pocket Sequence" $ do
  it "Open eyes works in isolation" testOpenEyesWorks
--  it "Initial look in pocket works (pocket accessible)" testInitialLookInPocket
  it "Complete sequence: open eyes → get robe → look in pocket works" testLookInPocketSequence
  it "Perception progression through the sequence" testPerceptionProgression

main :: IO ()
main = hspec spec
