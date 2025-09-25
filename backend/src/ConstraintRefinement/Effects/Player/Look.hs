module ConstraintRefinement.Effects.Player.Look where

import Model.Core (Effect (NarrationEffect), NarrationComputation (StaticNarration))
import Data.Text (Text)

-- For look errors (dynamic text based on error message)
lookErrorEffect :: Text -> Effect
lookErrorEffect errorMsg = NarrationEffect (StaticNarration errorMsg)