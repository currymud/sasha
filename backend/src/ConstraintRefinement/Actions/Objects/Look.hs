module ConstraintRefinement.Actions.Objects.Look (objectCannotBeSeenF,objectCanBeSeenF, containerCanBeSeenInF,containerCannotBeSeenInF) where
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ContainerDirectionalStimulusContainerActionF (ContainerCanBeSeenInF, ContainerCannotBeSeenInF'),
                                             ObjectDirectionalStimulusActionF (ObjectCanBeSeenF, ObjectCannotBeSeenF'))

objectCanBeSeenF :: ObjectDirectionalStimulusActionF
objectCanBeSeenF = ObjectCanBeSeenF processEffectsFromRegistry

objectCannotBeSeenF :: ObjectDirectionalStimulusActionF
objectCannotBeSeenF = ObjectCannotBeSeenF' processEffectsFromRegistry

containerCanBeSeenInF :: ContainerDirectionalStimulusContainerActionF
containerCanBeSeenInF = ContainerCanBeSeenInF processEffectsFromRegistry

containerCannotBeSeenInF :: ContainerDirectionalStimulusContainerActionF
containerCannotBeSeenInF = ContainerCannotBeSeenInF' processEffectsFromRegistry
