module ConstraintRefinement.Effects.Objects.Robe.Look where

import Model.Core (Effect (NarrationEffect), NarrationComputation (LookAtNarration, ContainerContentsNarration, StaticNarration), Object)
import Model.GID (GID)

robeLookNarrationEffect :: GID Object -> Effect
robeLookNarrationEffect robeGID = NarrationEffect (LookAtNarration robeGID)

robeContentsNarrationEffect :: GID Object -> Effect
robeContentsNarrationEffect robeGID = NarrationEffect (ContainerContentsNarration robeGID)

-- From notEvenRobeF action
notEvenRobeEffect :: Effect
notEvenRobeEffect = NarrationEffect (StaticNarration "One thing at a time. You've just woken up and your eyes are all bleary unfocused and closed. Maybe open them up and go from there?")