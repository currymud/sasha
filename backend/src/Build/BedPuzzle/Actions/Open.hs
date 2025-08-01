module Build.BedPuzzle.Actions.Open where
import           Control.Exception                                            (handle)
import           Control.Monad.Error.Class                                    (throwError)
import           Control.Monad.Identity                                       (Identity)
import qualified Data.Map.Strict
import           Data.Set                                                     (Set,
                                                                               toList)
import           Data.Text                                                    (Text)
import           GameState                                                    (changeImplicit,
                                                                               getPlayerLocationGID,
                                                                               modifyLocationM,
                                                                               modifyNarration)
import           Grammar.Parser.Partitions.Verbs.ImplicitRegionalStimulusVerb (wait)
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb         (look)
import           Model.GameState                                              (ActionEffectKey,
                                                                               ActionEffectMap (ActionEffectMap),
                                                                               ActionManagement (_implicitStimulusActionManagement),
                                                                               Effect,
                                                                               GameComputation,
                                                                               ImplicitStimulusActionF (ImplicitStimulusActionF),
                                                                               Location (_locationActionManagement),
                                                                               SomaticAccessActionF (SomaticAccessActionF),
                                                                               updateActionConsequence)
import           Model.GID                                                    (GID)
import           Model.Parser.Atomics.Verbs                                   (ImplicitStimulusVerb)


  {-
I need maps of GIDs to actions, to tell me how to handle
effects of opening eyes
     -}
openEyesDenied :: SomaticAccessActionF
openEyesDenied = SomaticAccessActionF (const(const denied))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "They're already open, relax."

openEyes :: SomaticAccessActionF
openEyes = SomaticAccessActionF opened
  where
    opened :: Set ActionEffectKey ->  ActionEffectMap -> GameComputation Identity ()
    opened actionEffectKeys (ActionEffectMap actionEffectMap) = do
      mapM_ process (Data.Set.toList actionEffectKeys)
      where
        process :: ActionEffectKey -> GameComputation Identity ()
        process actionEffectKey = do
          case Data.Map.Strict.lookup actionEffectKey actionEffectMap of
            Nothing -> throwError "No effect for actionEffectKey found in actionEffectMap"
            Just effects -> mapM_ handleEffect effects
            where
              handleEffect :: Effect -> GameComputation Identity ()
              handleEffect _ = pure ()
--          modifyNarration (updateActionConsequence msg) -- changeImplicit look aid >> modifyNarration (updateActionConsequence msg)
    msg :: Text
    msg = "You open your eyes, and the world comes into focus."
