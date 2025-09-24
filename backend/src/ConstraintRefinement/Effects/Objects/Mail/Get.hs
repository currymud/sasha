module ConstraintRefinement.Effects.Objects.Mail.Get where

import Model.Core (Effect (NarrationEffect), NarrationComputation (StaticNarration))

-- From getFromMailF action
getFromMailEffect :: Effect
getFromMailEffect = NarrationEffect (StaticNarration "You are already have your mail. it'll probably be important later.")

-- From getMailDeniedF action
getMailDeniedEffect :: Effect
getMailDeniedEffect = NarrationEffect (StaticNarration "You can't reach it from your bed. You need to get up first.")

-- From stangeMailF action (typo preserved from original)
stangeMailEffect :: Effect
stangeMailEffect = NarrationEffect (StaticNarration "You stand up to go to the table, but you are still a bit dizzy and lay back down")