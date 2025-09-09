{-# LANGUAGE OverloadedStrings #-}

module Test.TestRunner where

import           Control.Monad.Except           (runExceptT)
import           Control.Monad.Identity         (runIdentity)
import           Control.Monad.Reader           (runReaderT)
import           Control.Monad.State            (gets, runStateT)
import           Data.Text                      (Text)

import           Examples.BedroomDemo.GameState (config, gameState)
import           Model.Core                     (GameComputation (..),
                                                 GameState, GameStateT (..),
                                                 GameT, _evaluation, _evaluator,
                                                 runGameT, transformToIO)
import           Relude.String.Conversion       (ToText (toText))
import           TopLevel                       (toGameComputation, trySentence)
-- Execute a player command against GameState and return updated state
executeCommand :: Text -> GameState -> IO (Either Text GameState)
executeCommand input gameState = do
  case trySentence input of
    Left err -> pure $ Left err
    Right sentence -> do
      let computation = toGameComputation sentence
          gameT = transformToIO computation
          readerT = runReaderT (runGameT gameT) config
          exceptT = runExceptT readerT
          stateT = runStateT (runGameStateT exceptT) gameState

      result <- stateT
      case result of
        (Left err, _)         -> pure $ Left err
        (Right _, finalState) -> pure $ Right finalState
