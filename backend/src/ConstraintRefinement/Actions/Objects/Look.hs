module ConstraintRefinement.Actions.Objects.Look (objectLookedAtF,objectLookedInF) where
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ObjectDirectionalStimulusActionF (ObjectDirectionalStimulusActionF),
                                             ObjectDirectionalStimulusContainerActionF (ObjectLookedInF))

objectLookedAtF :: ObjectDirectionalStimulusActionF
objectLookedAtF = ObjectDirectionalStimulusActionF processEffectsFromRegistry

objectLookedInF :: ObjectDirectionalStimulusContainerActionF
objectLookedInF = ObjectLookedInF processEffectsFromRegistry
