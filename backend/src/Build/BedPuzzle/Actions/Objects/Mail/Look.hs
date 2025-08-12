module Build.BedPuzzle.Actions.Objects.Mail.Look where
import           Control.Monad.Identity (Identity)
import           Data.Text              (Text)
import           GameState              (modifyNarration)
import           Model.GameState        (DirectionalStimulusActionF (DirectionalStimulusActionF),
                                         GameComputation,
                                         updateActionConsequence)


-- Player has open eyes, but still in bed
whatMail :: DirectionalStimulusActionF
whatMail = DirectionalStimulusActionF (const (const whatMail'))
  where
    whatMail' ::GameComputation Identity ()
    whatMail'  = modifyNarration $ updateActionConsequence msg

    msg :: Text
    msg = "You should get out of bed first, then you can check the mail."

notEvenMail :: DirectionalStimulusActionF
notEvenMail = DirectionalStimulusActionF (const (const notEvenMail'))
  where
    notEvenMail' :: GameComputation Identity ()
    notEvenMail' = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "It's a slow start to the day, your eyes aren't even open yet, let alone seeing any mail."

seeMail :: DirectionalStimulusActionF
seeMail = DirectionalStimulusActionF (const (const seeMail'))
  where
    seeMail' :: GameComputation Identity ()
    seeMail' = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "It's junk mail, nothing important. However, you have a suspicion that you should GET MAIL before leaving the room."
