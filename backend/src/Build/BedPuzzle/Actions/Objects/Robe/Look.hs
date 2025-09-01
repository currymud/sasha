module Build.BedPuzzle.Actions.Objects.Robe.Look (whatRobe,notEvenRobe) where
import           Control.Monad.Identity (Identity)
import           Data.Text              (Text)
import           GameState              (modifyNarration)
import           Model.GameState        (DirectionalStimulusActionF (CannotSeeF),
                                         GameComputation,
                                         updateActionConsequence)

whatRobe :: DirectionalStimulusActionF
whatRobe = CannotSeeF whatPill'
  where
    whatPill' ::GameComputation Identity ()
    whatPill'  = modifyNarration $ updateActionConsequence msg

    msg :: Text
    msg = "You don't see you're robe here. What are you wearing, exactly?"

notEvenRobe :: DirectionalStimulusActionF
notEvenRobe = CannotSeeF notEvenPill'
  where
    notEvenPill' :: GameComputation Identity ()
    notEvenPill' = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "One thing at a time. You've just woken up and your eyes are all bleary unfocused and closed. Maybe open them up and go from there?"
