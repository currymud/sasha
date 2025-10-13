module ConstraintRefinement.Actions.Objects.Look (cannotBeSeenF,lookedAtF) where
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (DirectionalStimulusActionF (ObjectCannotBeSeenF, ObjectDirectionalStimulusActionF))



lookedAtF :: DirectionalStimulusActionF
lookedAtF = ObjectDirectionalStimulusActionF processEffectsFromRegistry
-- In the case where the player is capable of seeing,
-- and the location provides clear sight,
-- the object itself may not be visible
cannotBeSeenF :: DirectionalStimulusActionF
cannotBeSeenF = ObjectCannotBeSeenF processEffectsFromRegistry
--    msg :: Text
--    msg = "One thing at a time. You've just woken up and your eyes are all bleary unfocused and closed. Maybe open them up and go from there?"
