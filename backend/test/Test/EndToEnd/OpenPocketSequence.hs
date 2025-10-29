{-# LANGUAGE OverloadedStrings #-}

{-
End-to-End Test - Open Pocket Sequence
Tests the complete sequence: "open eyes" → "get robe" → "open pocket"
Expected final result: Pocket contents are revealed with pill visible
-}

module Test.EndToEnd.OpenPocketSequence (spec) where

import qualified Data.List           (isInfixOf)
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

-- Test the exact sequence: open eyes → get robe → open pocket
testOpenPocketSequence :: IO ()
testOpenPocketSequence = do
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
          hasRobeInInventory robeState `shouldBe` True

          -- Step 3: "open pocket"
          pocketResult <- executeCommand "open pocket" robeState
          case pocketResult of
            Left err -> expectationFailure $ "Open pocket failed: " <> show err
            Right finalState -> do
              let finalConsequences = _actionConsequence (_narration finalState)
              -- Should contain pill visibility messages
              finalConsequences `shouldSatisfy` any (T.isInfixOf "<OBJ-005>pill")
              finalConsequences `shouldSatisfy` any (T.isInfixOf "You open the pocket.")

-- Test that opening pocket reveals pill contents
testPocketContentsRevealed :: IO ()
testPocketContentsRevealed = do
  -- Execute the full sequence
  eyesResult <- executeCommand "open eyes" gameState
  case eyesResult of
    Left err -> expectationFailure $ "Open eyes failed: " <> show err
    Right eyesState -> do
      robeResult <- executeCommand "get robe" eyesState
      case robeResult of
        Left err -> expectationFailure $ "Get robe failed: " <> show err
        Right robeState -> do
          pocketResult <- executeCommand "open pocket" robeState
          case pocketResult of
            Left err -> expectationFailure $ "Open pocket failed: " <> show err
            Right finalState -> do
              let consequences = _actionConsequence (_narration finalState)
              -- Check for specific expected messages
              consequences `shouldSatisfy` any (T.isInfixOf "In it you see: <OBJ-005>pill")
              consequences `shouldSatisfy` any (T.isInfixOf "The pocket is open, revealing a pill.")
              consequences `shouldSatisfy` any (T.isInfixOf "Inside it you see: <OBJ-005>pill")

-- Test that pocket cannot be opened before getting robe
testPocketAccessDeniedWithoutRobe :: IO ()
testPocketAccessDeniedWithoutRobe = do
  eyesResult <- executeCommand "open eyes" gameState
  case eyesResult of
    Left err -> expectationFailure $ "Open eyes failed: " <> show err
    Right eyesState -> do
      pocketResult <- executeCommand "open pocket" eyesState
      case pocketResult of
        Left err -> expectationFailure $ "Open pocket should be accessible but denied: " <> show err
        Right finalState -> do
          let consequences = _actionConsequence (_narration finalState)
          consequences `shouldSatisfy` any (T.isInfixOf "You cannot reach the pocket.")

spec :: Spec
spec = describe "Open Pocket Sequence" $ do
  it "Complete sequence: open eyes → get robe → open pocket works" testOpenPocketSequence
  it "Opening pocket reveals pill contents correctly" testPocketContentsRevealed
  it "Pocket access denied without robe" testPocketAccessDeniedWithoutRobe

main :: IO ()
main = hspec spec