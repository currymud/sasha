module ConstraintRefinement.Effects.Objects.Pill.Take where

import Model.Core (Effect (NarrationEffect), NarrationComputation (StaticNarration))

-- From takePillDeniedF action
takePillDeniedEffect :: Effect
takePillDeniedEffect = NarrationEffect (StaticNarration "You can't take the pill right now.")

-- From alreadyTookPillF action
alreadyTookPillEffect :: Effect
alreadyTookPillEffect = NarrationEffect (StaticNarration "You already took the pill.")

-- From pillTooFarF action
pillTooFarEffect :: Effect
pillTooFarEffect = NarrationEffect (StaticNarration "You grab at it but it's hard to get to. try grabbing the robe first.")

-- From takePillF action (success message)
takePillSuccessEffect :: Effect
takePillSuccessEffect = NarrationEffect (StaticNarration "You take the pill and immediately feel better. Your headache is gone and you feel ready to get up.")