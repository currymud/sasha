module Build.BedPuzzle.Actions.Objects.Pill.Look where
import           Data.Text       (Text)
import           GameState       (modifyNarration)
import           Model.GameState (ActionF (DirectionalStimulusAction),
                                  updateActionConsequence)

whatPill :: ActionF
whatPill = DirectionalStimulusAction
  (modifyNarration $ updateActionConsequence msg)
  where
    msg :: Text
    msg = "You don't see a pill here, but you have a hazy memory of having one in your bathrobe pocket"

seePill :: ActionF
seePill = DirectionalStimulusAction
  (modifyNarration $ updateActionConsequence msg)
  where
    msg :: Text
    msg = "Your salvation in pill form! Cure your headache and get out of bed!"
