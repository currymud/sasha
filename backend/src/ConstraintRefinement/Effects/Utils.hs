module ConstraintRefinement.Effects.Utils where

import Model.Core (Effect (NarrationEffect), NarrationComputation (StaticNarration))
import Data.Text (Text)

-- For acquisition error handling - takes dynamic error text
acquisitionErrorEffect :: Text -> Effect
acquisitionErrorEffect errorText = NarrationEffect (StaticNarration errorText)