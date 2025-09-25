module ConstraintRefinement.Effects.Objects.Pill.Get where

import Model.Core (Effect (NarrationEffect), NarrationComputation (StaticNarration))

-- From alreadyHavePillF action
alreadyHavePillEffect :: Effect
alreadyHavePillEffect = NarrationEffect (StaticNarration "You already have the pill in your inventory.")

-- From getPillDeniedF action
getPillDeniedEffect :: Effect
getPillDeniedEffect = NarrationEffect (StaticNarration "You try but feel dizzy and have to lay back down")