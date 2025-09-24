module ConstraintRefinement.Effects.Objects.Robe.Get where

import Model.Core (Effect (NarrationEffect), NarrationComputation (StaticNarration))

-- From alreadyHaveRobeF action
alreadyHaveRobeEffect :: Effect
alreadyHaveRobeEffect = NarrationEffect (StaticNarration "You are already wearing the robe.")

-- From getRobeDeniedF action
getRobeDeniedEffect :: Effect
getRobeDeniedEffect = NarrationEffect (StaticNarration "You can't see it. You're dizzy with a hangover from the night before. Open your eyes.")