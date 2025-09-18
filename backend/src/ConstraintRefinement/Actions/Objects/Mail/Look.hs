module ConstraintRefinement.Actions.Objects.Mail.Look where
import           Control.Monad.Identity     (Identity)
import           Data.Text                  (Text)
import           GameState                  (modifyNarration,
                                             updateActionConsequence)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ActionEffectKey,
                                             DirectionalStimulusActionF (CannotSeeF),
                                             GameComputation)

whatMail :: DirectionalStimulusActionF
whatMail = CannotSeeF whatMail'
  where
    whatMail' :: ActionEffectKey -> GameComputation Identity ()
    whatMail' actionEffectKey = do
      processEffectsFromRegistry actionEffectKey
      modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You should get out of bed first, then you can check the mail."

notEvenMail :: DirectionalStimulusActionF
notEvenMail = CannotSeeF notEvenMail'
  where
    notEvenMail' :: ActionEffectKey -> GameComputation Identity ()
    notEvenMail' actionEffectKey = do
      processEffectsFromRegistry actionEffectKey
      modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "It's a slow start to the day, your eyes aren't even open yet, let alone seeing any mail."
