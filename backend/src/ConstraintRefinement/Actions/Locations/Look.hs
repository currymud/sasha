module ConstraintRefinement.Actions.Locations.Look where
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (LocationDirectionalStimulusActionF (LocationCanBeSeenF, LocationCannotBeSeenF),
                                             LocationDirectionalStimulusContainerActionF (LocationCanBeSeenInF, LocationCannotBeSeenInF),
                                             LocationImplicitStimulusActionF (LocationCanBeSeenImplicitF, LocationCannotBeSeenImplicitF))

locationNoSeeF :: LocationImplicitStimulusActionF
locationNoSeeF = LocationCannotBeSeenImplicitF processEffectsFromRegistry

allowLookF :: LocationImplicitStimulusActionF
allowLookF = LocationCanBeSeenImplicitF processEffectsFromRegistry

locationAllowLookAtF :: LocationDirectionalStimulusActionF
locationAllowLookAtF = LocationCanBeSeenF processEffectsFromRegistry

locationForbidLookAtF :: LocationDirectionalStimulusActionF
locationForbidLookAtF = LocationCannotBeSeenF processEffectsFromRegistry

locationForbidLookInF :: LocationDirectionalStimulusContainerActionF
locationForbidLookInF = LocationCannotBeSeenInF processEffectsFromRegistry

locationAllowLookInF :: LocationDirectionalStimulusContainerActionF
locationAllowLookInF = LocationCanBeSeenInF processEffectsFromRegistry
