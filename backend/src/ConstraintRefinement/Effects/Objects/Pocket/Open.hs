module ConstraintRefinement.Effects.Objects.Pocket.Open where

import Model.Core (Effect (NarrationEffect), NarrationComputation (StaticNarration))

-- From notEvenOpenF action
notEvenOpenEffect :: Effect
notEvenOpenEffect = NarrationEffect (StaticNarration "This is rather difficult as your eyes are closed")

-- From pocketOutOfReachF action
pocketOutOfReachEffect :: Effect
pocketOutOfReachEffect = NarrationEffect (StaticNarration "You'll need to get the robe first")