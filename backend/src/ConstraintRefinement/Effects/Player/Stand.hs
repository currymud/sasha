module ConstraintRefinement.Effects.Player.Stand where

import Model.Core (Effect (NarrationEffect), NarrationComputation (StaticNarration))

-- From Stand action - can't stand yet
cantStandEffect :: Effect
cantStandEffect = NarrationEffect (StaticNarration "You try to stand but the room starts spinning and you lay back down. There's some aspirin in your robe pocket.")

-- From Stand action - successfully standing
standSuccessEffect :: Effect
standSuccessEffect = NarrationEffect (StaticNarration "You stand up, feeling more alert and ready for action.")

-- From Stand action - already standing
alreadyStandingEffect :: Effect
alreadyStandingEffect = NarrationEffect (StaticNarration "You're already standing. No need to stand up again.")