module ConstraintRefinement.Actions.Locations.Look where
import           Data.Text            (Text)
import           GameState            (modifyNarration, updateActionConsequence)
import           GameState.Perception (youSeeM)
import           Model.Core           (ImplicitStimulusActionF (ImplicitStimulusActionF))

pitchBlackF :: ImplicitStimulusActionF
pitchBlackF = ImplicitStimulusActionF (const (const (modifyNarration $ updateActionConsequence pitchBlack))) -- agentCannotSee pitchBlack
  where
    pitchBlack :: Text
    pitchBlack = "It's pitch black, you can't see a thing."

lookF :: ImplicitStimulusActionF
lookF = ImplicitStimulusActionF (const (const youSeeM))
