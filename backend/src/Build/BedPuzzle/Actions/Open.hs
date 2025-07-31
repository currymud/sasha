module Build.BedPuzzle.Actions.Open where
import           Build.Identifiers.Actions                            (agentCanSeeGID)
import           Control.Monad.Identity                               (Identity)
import qualified Data.Map.Strict
import           Data.Text                                            (Text)
import           GameState                                            (changeImplicit,
                                                                       getPlayerLocationGID,
                                                                       modifyLocationM,
                                                                       modifyNarration)
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb (look)
import           Model.GameState                                      (ActionManagement (_implicitStimulusActionManagement),
                                                                       GameComputation,
                                                                       ImplicitStimulusActionF,
                                                                       Location (_locationActionManagement),
                                                                       SomaticAccessActionF (SomaticAccessActionF),
                                                                       updateActionConsequence)
import           Model.GID                                            (GID)
import           Model.Parser.Atomics.Verbs                           (ImplicitStimulusVerb)

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
    opened = changeImplicit look agentCanSeeGID >> modifyNarration (updateActionConsequence msg)
    msg :: Text
    msg = "You open your eyes, and the world comes into focus."
