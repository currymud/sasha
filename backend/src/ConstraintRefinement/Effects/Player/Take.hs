module ConstraintRefinement.Effects.Player.Take where

import Model.Core (Effect (NarrationEffect), NarrationComputation (StaticNarration))

-- From takeDeniedF action
takeDeniedEffect :: Effect
takeDeniedEffect = NarrationEffect (StaticNarration "You try but feel dizzy and have to lay back down")