module ConstraintRefinement.Effects.Objects.Floor.Get where

import Model.Core (Effect (NarrationEffect), NarrationComputation (StaticNarration))

-- From getFloorDeniedF action
getFloorDeniedEffect :: Effect
getFloorDeniedEffect = NarrationEffect (StaticNarration "You try to pick up the floor. It doesn't budge. Physics wins again.")