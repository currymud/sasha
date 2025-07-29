module Build.BedPuzzle.Actions.Objects.Pill.Look where
import           Data.Text       (Text)
import           GameState       (modifyNarration)
import           Model.GameState (DirectionalStimulusActionF (DirectionalStimulusActionF),
                                  updateActionConsequence)

whatPill :: DirectionalStimulusActionF
whatPill = DirectionalStimulusActionF
  (modifyNarration $ updateActionConsequence msg)
  where
    msg :: Text
    msg = "You don't see a pill here, but you have a hazy memory of having one in your bathrobe pocket"

seePill :: DirectionalStimulusActionF
seePill = DirectionalStimulusActionF
  (modifyNarration $ updateActionConsequence msg)
  where
    msg :: Text
    msg = "Your salvation in pill form! Cure your headache and get out of bed!"
