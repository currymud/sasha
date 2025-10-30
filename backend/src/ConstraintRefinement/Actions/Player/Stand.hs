module ConstraintRefinement.Actions.Player.Stand (standF) where
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (AgentPosturalActionF (AgentCanPosturalF))

standF :: AgentPosturalActionF
standF = AgentCanPosturalF processEffectsFromRegistry

--    msg :: Text
--    msg = "You try to stand but the room starts spinning and you lay back down. There's some aspirin in your robe pocket."

--    msg :: Text
--    msg = "You stand up, feeling more alert and ready for action."

--    msg :: Text
--    msg = "You're already standing. No need to stand up again."
