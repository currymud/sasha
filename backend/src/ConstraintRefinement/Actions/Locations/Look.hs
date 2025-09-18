module ConstraintRefinement.Actions.Locations.Look where
import           Control.Monad.Identity     (Identity)
import           Data.Text                  (Text)
import           GameState                  (modifyNarration,
                                             updateActionConsequence)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           GameState.Perception       (youSeeM)
import           Model.Core                 (ActionEffectKey, GameComputation,
                                             ImplicitStimulusActionF (CannotImplicitStimulusActionF, PlayerImplicitStimulusActionF))

pitchBlackF :: ImplicitStimulusActionF
pitchBlackF = CannotImplicitStimulusActionF pitchBlack'
  where
    pitchBlack' :: ActionEffectKey -> GameComputation Identity ()
    pitchBlack' actionEffectKey = do
      processEffectsFromRegistry actionEffectKey
      modifyNarration $ updateActionConsequence pitchBlack
    pitchBlack :: Text
    pitchBlack = "It's pitch black, you can't see a thing."

lookF :: ImplicitStimulusActionF
lookF = PlayerImplicitStimulusActionF look
  where
    look :: ActionEffectKey -> GameComputation Identity ()
    look actionEffectKey = do
      processEffectsFromRegistry actionEffectKey
      youSeeM
