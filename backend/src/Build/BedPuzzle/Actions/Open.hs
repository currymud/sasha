module Build.BedPuzzle.Actions.Open where
import           Control.Monad.Identity (Identity)
import           Data.Text              (Text)
import           GameState              (modifyNarration)
import           Model.GameState        (GameComputation,
                                         SomaticAccessActionF (SomaticAccessActionF),
                                         updateActionConsequence)

openEyesDenied :: SomaticAccessActionF
openEyesDenied = SomaticAccessActionF (const denied)
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "They're already open, relax."

openEyes :: SomaticAccessActionF
openEyes = SomaticAccessActionF (const opened)
  where
    opened :: GameComputation Identity ()
    opened = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You open your eyes, and the world comes into focus."
