module ConstraintRefinement.Actions.Locations.Look where
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (LocationDirectionalStimulusActionF (LocationDirectionalStimulusActionF),
                                             LocationDirectionalStimulusContainerActionF (LocationLookedInF),
                                             LocationImplicitStimulusActionF (LocationImplicitStimulusActionF))

locationLookF :: LocationImplicitStimulusActionF
locationLookF = LocationImplicitStimulusActionF processEffectsFromRegistry

locationLookAtF :: LocationDirectionalStimulusActionF
locationLookAtF = LocationDirectionalStimulusActionF processEffectsFromRegistry

locationLookInF :: LocationDirectionalStimulusContainerActionF
locationLookInF = LocationLookedInF processEffectsFromRegistry
