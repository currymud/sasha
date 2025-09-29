{-# LANGUAGE OverloadedStrings #-}

{-
End-to-End Test - Look at Robe Progression
Tests that "look at robe" behaves differently:
1. Before "open eyes" - should show darkness/cannot see
2. After "open eyes" - should show initial robe description  
3. After "get robe" - should show updated description
-}

module Test.EndToEnd.LookAtRobeProgression (spec) where

import           Data.Text      (Text, isInfixOf)
import           Test.Hspec

import           Examples.Initialize (gameState)
import           Model.Core          (GameState, _actionConsequence, _narration)
import           Test.TestRunner     (executeCommand)

-- Test "look at robe" before opening eyes
testLookAtRobeBeforeOpenEyes :: IO ()
testLookAtRobeBeforeOpenEyes = do
  result <- executeCommand "look at robe" gameState
  case result of
    Left err -> expectationFailure $ "Look at robe failed: " <> show err
    Right finalState -> do
      let consequences = _actionConsequence (_narration finalState)
      
      -- Should show darkness/eyes closed message
      consequences `shouldSatisfy` any (isInfixOf "eyes are all bleary")

-- Test "look at robe" after opening eyes
testLookAtRobeAfterOpenEyes :: IO ()
testLookAtRobeAfterOpenEyes = do
  -- First open eyes
  eyesResult <- executeCommand "open eyes" gameState
  case eyesResult of
    Left err -> expectationFailure $ "Open eyes failed: " <> show err
    Right eyesState -> do
      -- Then look at robe
      lookResult <- executeCommand "look at robe" eyesState
      case lookResult of
        Left err -> expectationFailure $ "Look at robe failed: " <> show err
        Right finalState -> do
          let consequences = _actionConsequence (_narration finalState)
          
          -- Should show initial robe description (supported by chair)
          consequences `shouldSatisfy` any (isInfixOf "is on the")

-- Test "look at robe" after getting it
testLookAtRobeAfterGetting :: IO ()
testLookAtRobeAfterGetting = do
  -- First open eyes
  eyesResult <- executeCommand "open eyes" gameState
  case eyesResult of
    Left err -> expectationFailure $ "Open eyes failed: " <> show err
    Right eyesState -> do
      -- Then get robe
      getRobeResult <- executeCommand "get robe" eyesState
      case getRobeResult of
        Left err -> expectationFailure $ "Get robe failed: " <> show err
        Right getRobeState -> do
          -- Then look at robe
          lookResult <- executeCommand "look at robe" getRobeState
          case lookResult of
            Left err -> expectationFailure $ "Look at robe failed: " <> show err
            Right finalState -> do
              let consequences = _actionConsequence (_narration finalState)
              
              -- Should show updated description with "make for wearin'"
              consequences `shouldSatisfy` any (isInfixOf "make for wearin'")

-- Test complete progression
testCompleteProgression :: IO ()
testCompleteProgression = do
  -- Step 1: Look at robe before opening eyes
  initialResult <- executeCommand "look at robe" gameState
  case initialResult of
    Left err -> expectationFailure $ "Initial look at robe failed: " <> show err
    Right initialState -> do
      let initialConsequences = _actionConsequence (_narration initialState)
      initialConsequences `shouldSatisfy` any (isInfixOf "eyes are all bleary")
      
      -- Step 2: Open eyes then look at robe
      eyesResult <- executeCommand "open eyes" gameState
      case eyesResult of
        Left err -> expectationFailure $ "Open eyes failed: " <> show err
        Right eyesState -> do
          eyesLookResult <- executeCommand "look at robe" eyesState
          case eyesLookResult of
            Left err -> expectationFailure $ "Look at robe after eyes failed: " <> show err
            Right eyesLookState -> do
              let eyesConsequences = _actionConsequence (_narration eyesLookState)
              eyesConsequences `shouldSatisfy` any (isInfixOf "is on the")
              
              -- Step 3: Get robe then look at it
              getRobeResult <- executeCommand "get robe" eyesState
              case getRobeResult of
                Left err -> expectationFailure $ "Get robe failed: " <> show err
                Right getRobeState -> do
                  finalLookResult <- executeCommand "look at robe" getRobeState
                  case finalLookResult of
                    Left err -> expectationFailure $ "Final look at robe failed: " <> show err
                    Right finalState -> do
                      let finalConsequences = _actionConsequence (_narration finalState)
                      finalConsequences `shouldSatisfy` any (isInfixOf "make for wearin'")

spec :: Spec
spec = describe "Look at Robe Progression" $ do
  it "Look at robe before open eyes shows darkness" testLookAtRobeBeforeOpenEyes
  it "Look at robe after open eyes shows spatial relationship" testLookAtRobeAfterOpenEyes  
  it "Look at robe after getting shows updated description" testLookAtRobeAfterGetting
  it "Complete progression through all three states" testCompleteProgression

main :: IO ()
main = hspec spec