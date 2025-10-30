module ConstraintRefinement.Actions.Locations.Stand where

import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (LocationPosturalActionF (LocationCanPosturalF, LocationCannotPosturalF))

standLocationF :: LocationPosturalActionF
standLocationF = LocationCanPosturalF processEffectsFromRegistry

standLocationDeniedF :: LocationPosturalActionF
standLocationDeniedF = LocationCannotPosturalF processEffectsFromRegistry
