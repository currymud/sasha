module ConstraintRefinement.Effects.Objects.Pill.Look where

import Model.Core (Effect (NarrationEffect), NarrationComputation (StaticNarration))

-- From whatPill action
whatPillEffect :: Effect
whatPillEffect = NarrationEffect (StaticNarration "You don't see a pill here, but you have a hazy memory of having one in your bathrobe pocket")

-- From notEvenPill action
notEvenPillEffect :: Effect
notEvenPillEffect = NarrationEffect (StaticNarration "It's a slow start to the day, your eyes aren't even open yet, let alone seeing a pill.")