{-# LANGUAGE DerivingStrategies #-}

module Model.Processing
  ( -- * Core Processing Types (moved to Model.Core)
    -- Evaluator(..), TriggerRegistry(..)
    -- * Field Update Operations
    FieldUpdateOperation(..)
    -- * Verb Processing Types
  , ProcessDirectionalStimulusVerb(..)
  , ProcessImplicitStimulusVerb(..)
  , ProcessImplicitVerbMap
  , ProcessImplicitVerbMaps
  , PlayerProcessImplicitVerbMap
  ) where

import           Control.Monad.Identity (Identity)
import           Data.Map.Strict        (Map)
import           Data.Text              (Text)
import           Model.Core             (EffectActionKey, SystemEffect,
                                         SystemEffectConfig, SystemEffectKey,
                                         GameComputation, ImplicitStimulusActionF,
                                         Location, Object)
import           Model.GID              (GID)
import           Model.Parser           (Sentence)
import           Model.Parser.Composites.Nouns (DirectionalStimulusNounPhrase)
import           Model.Parser.Atomics.Verbs (ImplicitStimulusVerb, DirectionalStimulusVerb)

-- Evaluator and TriggerRegistry moved to Model.Core

-- | Operations for updating game state fields
data FieldUpdateOperation
  = ObjectShortName (GID Object) Text
  | ObjectDescription (GID Object) Text
  | LocationTitle (GID Location) Text
  | PlayerLocation (GID Location)
  deriving stock (Show, Eq, Ord)

type ProcessImplicitVerbMap = Map (GID ProcessImplicitStimulusVerb) (ImplicitStimulusVerb -> ImplicitStimulusActionF)

type ProcessImplicitVerbMaps = Map ImplicitStimulusVerb ProcessImplicitVerbMap

type PlayerProcessImplicitVerbMap = Map ImplicitStimulusVerb (GID ProcessImplicitStimulusVerb)

newtype ProcessImplicitStimulusVerb = ProcessImplicitStimulusVerb
  { _unProcessImplicitStimlusVerb :: ImplicitStimulusVerb -> GameComputation Identity ()}

newtype ProcessDirectionalStimulusVerb = ProcessDirectionalStimulusVerb
  { _unProcessDirectionalStimlusVerb :: DirectionalStimulusVerb
                                          -> DirectionalStimulusNounPhrase
                                          -> GameComputation Identity ()
  }
