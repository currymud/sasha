{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Model.Parser where

import           Data.Kind                     (Type)
import           Model.Parser.Atomics.Verbs    (CardinalMovementVerb,
                                                DirectionalStimulusVerb,
                                                ImplicitBoundaryVerb,
                                                ImplicitStimulusVerb,
                                                SimpleAccessVerb)
import           Model.Parser.Composites.Verbs (Imperative)

type Sentence :: Type
data Sentence
  = Imperative Imperative
  deriving stock (Eq,Ord,Show)

type ActionManagement :: Type
data ActionManagement
 = CardinalMovement CardinalMovementVerb
 | ImplicitBoundary ImplicitBoundaryVerb
 | ImplicitStimulus ImplicitStimulusVerb
 | DirectionalStimulus DirectionalStimulusVerb
 | SimpleAccess SimpleAccessVerb
 | ImplicitRegionalStimulus ImplicitStimulusVerb
 deriving stock (Eq,Ord,Show)
