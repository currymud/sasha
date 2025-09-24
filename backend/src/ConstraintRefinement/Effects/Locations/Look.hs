module ConstraintRefinement.Effects.Locations.Look where

import Model.Core (Effect (NarrationEffect), NarrationComputation (StaticNarration))

-- From pitchBlackF action
pitchBlackEffect :: Effect
pitchBlackEffect = NarrationEffect (StaticNarration "It's pitch black, you can't see a thing.")