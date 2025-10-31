module ConstraintRefinement.Actions.Objects.Look (objectLookedAtF,objectLookedInF) where
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ContainerDirectionalStimulusContainerActionF (ContainerLookedInF),
                                             ObjectDirectionalStimulusActionF (ObjectDirectionalStimulusActionF))

objectLookedAtF :: ObjectDirectionalStimulusActionF
objectLookedAtF = ObjectDirectionalStimulusActionF processEffectsFromRegistry

objectLookedInF :: ContainerDirectionalStimulusContainerActionF
objectLookedInF = ContainerLookedInF processEffectsFromRegistry
