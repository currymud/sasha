module ConstraintRefinement.Effects.Objects.Mail.Look where

import Model.Core (Effect (NarrationEffect), NarrationComputation (StaticNarration))

-- From whatMailF action
whatMailEffect :: Effect
whatMailEffect = NarrationEffect (StaticNarration "You should get out of bed first, then you can check the mail.")

-- From notEvenMailF action
notEvenMailEffect :: Effect
notEvenMailEffect = NarrationEffect (StaticNarration "It's a slow start to the day, your eyes aren't even open yet, let alone seeing any mail.")