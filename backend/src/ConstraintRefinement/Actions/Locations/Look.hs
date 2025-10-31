module ConstraintRefinement.Actions.Locations.Look where
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (LocationDirectionalStimulusActionF (LocationDirectionalStimulusActionF),
                                             LocationDirectionalStimulusContainerActionF (LocationCanBeSeenInF, LocationCannotBeSeenInF),
                                             LocationImplicitStimulusActionF (LocationImplicitStimulusActionF))

locationLookF :: LocationImplicitStimulusActionF
locationLookF = LocationImplicitStimulusActionF processEffectsFromRegistry

locationLookAtF :: LocationDirectionalStimulusActionF
locationLookAtF = LocationDirectionalStimulusActionF processEffectsFromRegistry

locationForbidLookInF :: LocationDirectionalStimulusContainerActionF
locationForbidLookInF = LocationCannotBeSeenInF processEffectsFromRegistry

locationAllowLookInF :: LocationDirectionalStimulusContainerActionF
locationAllowLookInF = LocationCanBeSeenInF processEffectsFromRegistry
