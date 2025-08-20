module Build.BedPuzzle.Actions.Look where
import           Actions.Percieve.Look (agentCannotSee)
import           Data.Text             (Text)
import           GameState             (modifyNarration)
import           Model.GameState       (ImplicitStimulusActionF (ImplicitStimulusActionF),
                                        updateActionConsequence)


pitchBlackF :: ImplicitStimulusActionF
pitchBlackF = ImplicitStimulusActionF (const (const (modifyNarration $ updateActionConsequence pitchBlack))) -- agentCannotSee pitchBlack
  where
    pitchBlack :: Text
    pitchBlack = "It's pitch black, you can't see a thing."
