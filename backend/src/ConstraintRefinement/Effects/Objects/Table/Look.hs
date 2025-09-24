module ConstraintRefinement.Effects.Objects.Table.Look where

import Model.Core (Effect (NarrationEffect), NarrationComputation (StaticNarration))

-- From whatTableF action
whatTableEffect :: Effect
whatTableEffect = NarrationEffect (StaticNarration "It would be alot easier to see the table if you would open your eyes. Literally.")

-- From notEvenTableF action
notEvenTableEffect :: Effect
notEvenTableEffect = NarrationEffect (StaticNarration "You see a table. It is a table. It is not a very interesting table, but it is a table nonetheless.")