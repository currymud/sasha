module ConstraintRefinement.Actions.Objects.Look (objectCannotBeSeenF,objectCanBeSeenF) where
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ObjectDirectionalStimulusActionF (ObjectCanBeSeenF, ObjectCannotBeSeenF'))

objectCanBeSeenF :: ObjectDirectionalStimulusActionF
objectCanBeSeenF = ObjectCanBeSeenF processEffectsFromRegistry

objectCannotBeSeenF :: ObjectDirectionalStimulusActionF
objectCannotBeSeenF = ObjectCannotBeSeenF' processEffectsFromRegistry
