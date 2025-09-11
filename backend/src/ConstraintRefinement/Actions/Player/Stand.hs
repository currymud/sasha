module ConstraintRefinement.Actions.Player.Stand (standDenied, standUp) where
import           Control.Monad.Identity (Identity)
import           Data.Set               (Set)
import           Data.Text              (Text)
import           GameState              (modifyNarration,
                                         updateActionConsequence)
import           Model.Core             (ActionEffectMap (ActionEffectMap),
                                         EffectTargetKey, GameComputation,
                                         PosturalActionF (PosturalActionF))

standDenied :: PosturalActionF
standDenied = PosturalActionF (const (const denied))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You try to stand but the room starts spinning and you lay back down. There's some aspirin in your robe pocket."

standUp :: PosturalActionF
standUp = PosturalActionF stood
  where
    stood :: Set EffectTargetKey -> ActionEffectMap -> GameComputation Identity ()
    stood actionEffectKeys (ActionEffectMap actionEffectMap) = do
      -- Processsing ToDo
      modifyNarration (updateActionConsequence msg)

msg :: Text
msg = "You stand up, feeling more alert and ready for action."

standUpDenied :: PosturalActionF
standUpDenied = PosturalActionF (const (const denied))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You're already standing. No need to stand up again."
