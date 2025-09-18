module ConstraintRefinement.Actions.Objects.Pill.Look where
import           Control.Monad.Identity     (Identity)
import           Data.Text                  (Text)
import           GameState                  (modifyNarration,
                                             updateActionConsequence)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ActionEffectKey,
                                             DirectionalStimulusActionF (CannotSeeF),
                                             GameComputation)

whatPill :: DirectionalStimulusActionF
whatPill = CannotSeeF whatPill'
  where
    whatPill' :: ActionEffectKey -> GameComputation Identity ()
    whatPill' actionEffectKey = do
      processEffectsFromRegistry actionEffectKey
      modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You don't see a pill here, but you have a hazy memory of having one in your bathrobe pocket"

notEvenPill :: DirectionalStimulusActionF
notEvenPill = CannotSeeF notEvenPill'
  where
    notEvenPill' :: ActionEffectKey -> GameComputation Identity ()
    notEvenPill' actionEffectKey = do
      processEffectsFromRegistry actionEffectKey
      modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "It's a slow start to the day, your eyes aren't even open yet, let alone seeing a pill."
