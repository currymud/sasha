module Build.BedPuzzle.Actions.Open where
import           Control.Exception                                    (handle)
import           Control.Monad.Identity                               (Identity)
import qualified Data.Map.Strict
import           Data.Text                                            (Text)
import           GameState                                            (changeImplicit,
                                                                       getPlayerLocationGID,
                                                                       modifyLocationM,
                                                                       modifyNarration)
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb (look)
import           Model.GameState                                      (ActionEffectMap,
                                                                       ActionManagement (_implicitStimulusActionManagement),
                                                                       GameComputation,
                                                                       ImplicitStimulusActionF (ImplicitStimulusActionF),
                                                                       Location (_locationActionManagement),
                                                                       SomaticAccessActionF (SomaticAccessActionF),
                                                                       updateActionConsequence)
import           Model.GID                                            (GID)
import           Model.Parser.Atomics.Verbs                           (ImplicitStimulusVerb)


  {-
I need maps of GIDs to actions, to tell me how to handle
effects of opening eyes
     -}
openEyesDenied :: SomaticAccessActionF
openEyesDenied = SomaticAccessActionF (const denied)
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "They're already open, relax."

openEyes :: SomaticAccessActionF
openEyes = SomaticAccessActionF opened
  where
    opened :: ActionEffectMap -> GameComputation Identity ()
    opened _ = modifyNarration (updateActionConsequence msg) -- changeImplicit look aid >> modifyNarration (updateActionConsequence msg)
    msg :: Text
    msg = "You open your eyes, and the world comes into focus."
