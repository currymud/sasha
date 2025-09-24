module ConstraintRefinement.Effects.Player.Get where

import Model.Core (Effect (NarrationEffect), NarrationComputation (StaticNarration))

getObjectNarrationEffect :: Effect
getObjectNarrationEffect = NarrationEffect (StaticNarration "You pick it up.")

-- From getDeniedF action
getDeniedEffect :: Effect
getDeniedEffect = NarrationEffect (StaticNarration "You try but feel dizzy and have to lay back down")