module ConstraintRefinement.Actions.Objects.Mail.Look where
import           Control.Monad.Identity     (Identity)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ActionEffectKey,
                                             DirectionalStimulusActionF (CannotSeeF),
                                             GameComputation)

whatMail :: DirectionalStimulusActionF
whatMail = CannotSeeF whatMail'
  where
    whatMail' :: ActionEffectKey -> GameComputation Identity ()
    whatMail' actionEffectKey = processEffectsFromRegistry actionEffectKey

notEvenMail :: DirectionalStimulusActionF
notEvenMail = CannotSeeF notEvenMail'
  where
    notEvenMail' :: ActionEffectKey -> GameComputation Identity ()
    notEvenMail' actionEffectKey = processEffectsFromRegistry actionEffectKey
