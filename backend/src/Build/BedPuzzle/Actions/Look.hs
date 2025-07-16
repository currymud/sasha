module Build.BedPuzzle.Actions.Look where
import           Actions.Percieve.Look (agentCannotSee)
import           Data.Text             (Text)
import           Model.GameState       (ActionF)


pitchBlackF :: ActionF
pitchBlackF =  agentCannotSee pitchBlack
  where
    pitchBlack :: Text
    pitchBlack = "It's pitch black, you can't see a thing."
