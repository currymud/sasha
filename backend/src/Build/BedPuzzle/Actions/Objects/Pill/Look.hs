module Build.BedPuzzle.Actions.Objects.Pill.Look where
import           Control.Monad.Identity (Identity)
import           Data.Text              (Text)
import           GameState              (modifyNarration)
import           Model.GameState        (DirectionalStimulusActionF (DirectionalStimulusActionF),
                                         GameComputation (GameComputation),
                                         Location, updateActionConsequence)

whatPill :: DirectionalStimulusActionF
whatPill = DirectionalStimulusActionF (const (const whatPill'))
  where
    whatPill' ::GameComputation Identity ()
    whatPill'  = modifyNarration $ updateActionConsequence msg

    msg :: Text
    msg = "You don't see a pill here, but you have a hazy memory of having one in your bathrobe pocket"

seePill :: DirectionalStimulusActionF
seePill = DirectionalStimulusActionF (const (const seePill'))
  where
    seePill' :: GameComputation Identity ()
    seePill' = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "Your salvation in pill form! Cure your headache and get out of bed!"
