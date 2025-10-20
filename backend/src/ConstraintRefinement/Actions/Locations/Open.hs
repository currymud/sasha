module ConstraintRefinement.Actions.Locations.Open where

import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (LocationContainerAccessActionF (LocationCanAccessContainerF, LocationCannotAccessContainerF))

openLocationF :: LocationContainerAccessActionF
openLocationF = LocationCanAccessContainerF processEffectsFromRegistry

openLocationDeniedF :: LocationContainerAccessActionF
openLocationDeniedF = LocationCannotAccessContainerF processEffectsFromRegistry
