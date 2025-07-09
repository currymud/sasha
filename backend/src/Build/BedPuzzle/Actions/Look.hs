module Build.BedPuzzle.Actions.Look where
import           Actions.Percieve.Look (agentCannotSee)
import           Data.Text             (Text)
import           Model.GameState       (ActionF (ImplicitStimulusF), Location,
                                        ResolutionF)


pitchBlackF :: ActionF ResolutionF
pitchBlackF = ImplicitStimulusF $ agentCannotSee pitchBlack
  where
    pitchBlack :: Text
    pitchBlack = "It's pitch black, you can't see a thing."


