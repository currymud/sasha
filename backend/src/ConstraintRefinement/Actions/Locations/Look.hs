module ConstraintRefinement.Actions.Locations.Look where
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (LocationDirectionalStimulusActionF (LocationCanBeSeenF, LocationCannotBeSeenF),
                                             LocationDirectionalStimulusContainerActionF (LocationCanBeSeenInF, LocationCannotBeSeenInF),
                                             LocationImplicitStimulusActionF (LocationImplicitStimulusActionF))

locationLookF :: LocationImplicitStimulusActionF
locationLookF = LocationImplicitStimulusActionF processEffectsFromRegistry

locationAllowLookAtF :: LocationDirectionalStimulusActionF
locationAllowLookAtF = LocationCanBeSeenF processEffectsFromRegistry

locationForbidLookAtF :: LocationDirectionalStimulusActionF
locationForbidLookAtF = LocationCannotBeSeenF processEffectsFromRegistry

locationForbidLookInF :: LocationDirectionalStimulusContainerActionF
locationForbidLookInF = LocationCannotBeSeenInF processEffectsFromRegistry

locationAllowLookInF :: LocationDirectionalStimulusContainerActionF
locationAllowLookInF = LocationCanBeSeenInF processEffectsFromRegistry
