module ConstraintRefinement.Effects.Objects.Pocket.Look where

import Model.Core (Effect (NarrationEffect), NarrationComputation (LookAtNarration, ContainerContentsNarration, StaticNarration), Object)
import Model.GID (GID)

pocketLookNarrationEffect :: GID Object -> Effect
pocketLookNarrationEffect pocketGID = NarrationEffect (LookAtNarration pocketGID)

pocketContentsNarrationEffect :: GID Object -> Effect
pocketContentsNarrationEffect pocketGID = NarrationEffect (ContainerContentsNarration pocketGID)

-- From whatPocket action
whatPocketEffect :: Effect
whatPocketEffect = NarrationEffect (StaticNarration "Pocket? What Pocket?")

-- From notEvenPocket action
notEvenPocketEffect :: Effect
notEvenPocketEffect = NarrationEffect (StaticNarration "One thing at a time. You've just woken up and your eyes are all bleary unfocused and closed. Maybe open them up and go from there?")

-- From pocketClosedF action
pocketClosedEffect :: Effect
pocketClosedEffect = NarrationEffect (StaticNarration "The pocket is velcroed shut.")