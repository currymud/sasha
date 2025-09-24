module ConstraintRefinement.Effects.Player.Open where

import Model.Core (Effect (NarrationEffect), NarrationComputation (StaticNarration))

-- From openDenied action
openDeniedEffect :: Effect
openDeniedEffect = NarrationEffect (StaticNarration "You are in position to not be opening anything but your eyes.")

-- From eyesAlreadyOpen action
eyesAlreadyOpenEffect :: Effect
eyesAlreadyOpenEffect = NarrationEffect (StaticNarration "They're already open, relax.")

-- From openEyes action
openEyesSuccessEffect :: Effect
openEyesSuccessEffect = NarrationEffect (StaticNarration "You open your eyes, and the world comes into focus.")