{-# LANGUAGE OverloadedStrings #-}

module Test.TestRunner where

import           Control.Monad.Except           (runExceptT)
import           Control.Monad.Reader           (runReaderT)
import           Control.Monad.State            (get, runStateT)
import           Data.Text                      (Text)

import           Examples.BedroomDemo.GameState (config, gameState)
import           GameState                      (clearNarration)
import           Model.Core                     (GameState, GameStateT (..),
                                                 GameT, runGameT, transformToIO)
import           TopLevel                       (processWithSystemEffects,
                                                 trySentence)

-- Execute a player command like topLevel but with Text input, no loop, returns GameState
executeCommandInGameT :: Text -> GameT IO GameState
executeCommandInGameT input = do
  transformToIO clearNarration  -- Clear narration first like topLevel
  case trySentence input of
    Left err -> error $ show err  -- Handle parse errors
    Right sentence -> do
      transformToIO $ processWithSystemEffects sentence
      get  -- Return the GameState

-- Execute a player command against GameState and return updated state
executeCommand :: Text -> GameState -> IO (Either Text GameState)
executeCommand input gameState = do
  let gameT = executeCommandInGameT input
      readerT = runReaderT (runGameT gameT) config
      exceptT = runExceptT readerT
      stateT = runStateT (runGameStateT exceptT) gameState

  result <- stateT
  case result of
    (Left err, _)         -> pure $ Left err
    (Right finalState, _) -> pure $ Right finalState
