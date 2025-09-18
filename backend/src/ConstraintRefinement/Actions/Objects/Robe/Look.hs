module ConstraintRefinement.Actions.Objects.Robe.Look (notEvenRobeF) where
import           Control.Monad.Identity     (Identity)
import           Data.Text                  (Text)
import           GameState                  (modifyNarration,
                                             updateActionConsequence)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ActionEffectKey,
                                             DirectionalStimulusActionF (CannotSeeF),
                                             GameComputation)

notEvenRobeF :: DirectionalStimulusActionF
notEvenRobeF = CannotSeeF notEvenPill'
  where
    notEvenPill' :: ActionEffectKey -> GameComputation Identity ()
    notEvenPill' actionEffectKey = do
      processEffectsFromRegistry actionEffectKey
      modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "One thing at a time. You've just woken up and your eyes are all bleary unfocused and closed. Maybe open them up and go from there?"
