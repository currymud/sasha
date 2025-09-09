{-# LANGUAGE OverloadedStrings #-}

{-
End-to-End Test - Phase 3: Sequential Command Execution
Tests that after "open eyes", "get robe" succeeds with multi-entity coordination
-}

module Test.EndToEnd.OpenEyesUnlocksGetRobe (spec) where

import qualified Data.Map.Strict                as Map
import qualified Data.Set                       as Set
import           Data.Text                      (Text, isInfixOf)
import           Test.Hspec

import           Examples.BedroomDemo.GameState (gameState)
import           Model.Core                     (GameState, _actionConsequence,
                                                 _inventory, _narration,
                                                 _objectMap, _player,
                                                 _shortName, _world)
import           Model.Core.Mappings            (_getGIDToDataMap)
import           Test.TestRunner                (executeCommand)

-- Helper function to check if robe is in inventory
hasRobeInInventory :: GameState -> Bool
hasRobeInInventory gs =
  let inventory = _inventory (_player gs)
      objectMap = (_getGIDToDataMap . _objectMap) (_world gs)
  in any (\objGID ->
    case Map.lookup objGID objectMap of
      Just obj -> "<OBJ-003>" `isInfixOf` _shortName obj
      Nothing  -> False) inventory

-- Test sequential execution: "open eyes" then "get robe"
testOpenEyesThenGetRobe :: IO ()
testOpenEyesThenGetRobe = do
  -- Execute "open eyes" first
  eyesResult <- executeCommand "open eyes" gameState
  case eyesResult of
    Left err -> expectationFailure $ "Open eyes failed: " <> show err
    Right eyesState -> do
      -- Execute "get robe" on the updated state
      robeResult <- executeCommand "get robe" eyesState
      case robeResult of
        Left err -> expectationFailure $ "Get robe failed: " <> show err
        Right finalState -> do
          let consequences = _actionConsequence (_narration finalState)

          consequences `shouldSatisfy` (== getRobeSuccess)
          hasRobeInInventory finalState `shouldBe` True
  where
    getRobeSuccess :: [Text]
    getRobeSuccess = ["You pick it up."]

-- Test that inventory was initially empty
testInitialInventoryEmpty :: IO ()
testInitialInventoryEmpty = do
  let inventory = _inventory (_player gameState)
  inventory `shouldSatisfy` Set.null

-- Test complete flow from initial failure to success
testCompleteFlow :: IO ()
testCompleteFlow = do
  -- Initial "get robe" should fail
  initialResult <- executeCommand "get robe" gameState
  case initialResult of
    Left err -> expectationFailure $ "Initial get robe command failed: " <> show err
    Right initialState -> do
      let initialConsequences = _actionConsequence (_narration initialState)
      initialConsequences `shouldSatisfy` (== initialFailure)

      -- "open eyes" should succeed
      eyesResult <- executeCommand "open eyes" gameState
      case eyesResult of
        Left err -> expectationFailure $ "Open eyes failed: " <> show err
        Right eyesState -> do
          -- "get robe" should now succeed
          finalResult <- executeCommand "get robe" eyesState
          case finalResult of
            Left err -> expectationFailure $ "Final get robe failed: " <> show err
            Right finalState -> do
              let finalConsequences = _actionConsequence (_narration finalState)
              finalConsequences `shouldSatisfy` (== finalSuccess)
              hasRobeInInventory finalState `shouldBe` True
  where
    initialFailure :: [Text]
    initialFailure = ["You try but feel dizzy and have to lay back down"]
    finalSuccess :: [Text]
    finalSuccess = ["You pick it up."]

spec :: Spec
spec = describe "Phase 3: Sequential Command Execution" $ do
  it "Initial inventory is empty" testInitialInventoryEmpty
  it "Open eyes then get robe succeeds" testOpenEyesThenGetRobe
  it "Complete flow: initial failure → open eyes → success" testCompleteFlow

main :: IO ()
main = hspec spec
