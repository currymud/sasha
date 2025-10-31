module ConstraintRefinement.Actions.Objects.Look (objectLookedAtF,containerCanBeSeenInF,containerCannotBeSeenInF) where
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ContainerDirectionalStimulusContainerActionF (ContainerCanBeSeenInF, ContainerCannotBeSeenInF'),
                                             ObjectDirectionalStimulusActionF (ObjectDirectionalStimulusActionF))

objectLookedAtF :: ObjectDirectionalStimulusActionF
objectLookedAtF = ObjectDirectionalStimulusActionF processEffectsFromRegistry

containerCanBeSeenInF :: ContainerDirectionalStimulusContainerActionF
containerCanBeSeenInF = ContainerCanBeSeenInF processEffectsFromRegistry

containerCannotBeSeenInF :: ContainerDirectionalStimulusContainerActionF
containerCannotBeSeenInF = ContainerCannotBeSeenInF' processEffectsFromRegistry
