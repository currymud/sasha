module ConstraintRefinement.Actions.Player.Stand (standDenied, standUp,alreadyStandingF) where
import           Control.Monad.Identity (Identity)
import           Data.Set               (Set)
import           Data.Text              (Text)
import           GameState              (modifyNarration,
                                         updateActionConsequence)
import           Model.Core             (GameComputation,
                                         PosturalActionF (CannotPosturalActionF, PlayerPosturalActionF),
                                         TargetEffectKey,
                                         TargetEffectMap (TargetEffectMap))

standDenied :: PosturalActionF
standDenied = CannotPosturalActionF denied
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You try to stand but the room starts spinning and you lay back down. There's some aspirin in your robe pocket."

standUp :: PosturalActionF
standUp = PlayerPosturalActionF stood
  where
    stood :: Set TargetEffectKey -> TargetEffectMap -> GameComputation Identity ()
    stood actionEffectKeys (TargetEffectMap actionEffectMap) = do
      -- Processsing ToDo
      modifyNarration (updateActionConsequence msg)

msg :: Text
msg = "You stand up, feeling more alert and ready for action."

alreadyStandingF :: PosturalActionF
alreadyStandingF = CannotPosturalActionF denied
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You're already standing. No need to stand up again."
