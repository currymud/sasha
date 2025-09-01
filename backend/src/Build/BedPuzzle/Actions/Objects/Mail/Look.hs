module Build.BedPuzzle.Actions.Objects.Mail.Look where
import           Control.Monad.Identity (Identity)
import           Data.Text              (Text)
import           GameState              (modifyNarration)
import           Model.GameState        (DirectionalStimulusActionF (CannotSeeF),
                                         GameComputation,
                                         updateActionConsequence)

-- | Naming convention mf'r. Do you know it?
-- Player has open eyes, but still in bed
whatMail :: DirectionalStimulusActionF
whatMail = CannotSeeF whatMail'
  where
    whatMail' ::GameComputation Identity ()
    whatMail'  = modifyNarration $ updateActionConsequence msg

    msg :: Text
    msg = "You should get out of bed first, then you can check the mail."

notEvenMail :: DirectionalStimulusActionF
notEvenMail = CannotSeeF notEvenMail'
  where
    notEvenMail' :: GameComputation Identity ()
    notEvenMail' = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "It's a slow start to the day, your eyes aren't even open yet, let alone seeing any mail."
