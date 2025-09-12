module ConstraintRefinement.Actions.Locations.Look where
import           Control.Monad.Identity (Identity)
import           Data.Text              (Text)
import           GameState              (modifyNarration,
                                         updateActionConsequence)
import           GameState.Perception   (youSeeM)
import           Model.Core             (ActionEffectKey, GameComputation,
                                         ImplicitStimulusActionF (CannotImplicitStimulusActionF, PlayerImplicitStimulusActionF),
                                         ImplicitStimulusActionMap)

pitchBlackF :: ImplicitStimulusActionF
pitchBlackF = CannotImplicitStimulusActionF pitchBlack
  where
    pitchBlack :: ActionEffectKey
                    -> ImplicitStimulusActionMap
                    -> GameComputation Identity ()
    pitchBlack actionEffectKey implicitStimulusActionMap = do
      modifyNarration $ updateActionConsequence pitchBlackMsg
    pitchBlackMsg :: Text
    pitchBlackMsg = "It's pitch black, you can't see a thing."

lookF :: ImplicitStimulusActionF
lookF = PlayerImplicitStimulusActionF look
        where
          look :: ActionEffectKey
                    -> ImplicitStimulusActionMap
                    -> GameComputation Identity ()
          look actionEffectKey implicitStimulusActionMap = youSeeM
