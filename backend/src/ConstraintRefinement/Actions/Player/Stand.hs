module ConstraintRefinement.Actions.Player.Stand (standDenied, standUp,standUpDenied) where
import           Control.Monad.Identity     (Identity)
import           Data.Text                  (Text)
import           GameState                  (modifyNarration,
                                             updateActionConsequence)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ActionEffectKey, GameComputation,
                                             PosturalActionF (CannotPosturalActionF, PlayerPosturalActionF))

standDenied :: PosturalActionF
standDenied = CannotPosturalActionF denied
  where
    denied :: ActionEffectKey -> GameComputation Identity ()
    denied actionEffectKey = do
      processEffectsFromRegistry actionEffectKey
      modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You try to stand but the room starts spinning and you lay back down. There's some aspirin in your robe pocket."

standUp :: PosturalActionF
standUp = PlayerPosturalActionF stood
  where
    stood :: ActionEffectKey -> GameComputation Identity ()
    stood actionEffectKeys = do
      processEffectsFromRegistry actionEffectKeys
      modifyNarration (updateActionConsequence msg)
    msg :: Text
    msg = "You stand up, feeling more alert and ready for action."

standUpDenied :: PosturalActionF
standUpDenied = CannotPosturalActionF denied
  where
    denied :: ActionEffectKey -> GameComputation Identity ()
    denied actionEffectKey = do
      processEffectsFromRegistry actionEffectKey
      modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You're already standing. No need to stand up again."
