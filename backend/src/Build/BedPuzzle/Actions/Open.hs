{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Open where
import           Control.Monad.Error.Class     (throwError)
import           Control.Monad.Identity        (Identity)
import qualified Data.Map.Strict
import           Data.Set                      (Set, filter, insert, toList)
import           Data.Text                     (Text, pack)
import           GameState                     (modifyLocationM,
                                                modifyNarration,
                                                processAcquisitionEffect,
                                                processPosturalEffect)
import           GameState.Perception          (buildPerceptionMapFromObjects,
                                                computePerceivableObjects,
                                                modifyPerceptionMapM, youSeeM)
import           Model.GameState               (ActionEffectKey (LocationKey, ObjectKey, PlayerKey),
                                                ActionEffectMap (ActionEffectMap),
                                                ActionManagement (CAManagementKey, DSAManagementKey, ISAManagementKey, NPManagementKey, PPManagementKey, SSAManagementKey),
                                                ActionManagementFunctions (ActionManagementFunctions),
                                                Effect (AcquisitionEffect, ConsumptionEffect, DirectionalStimulusEffect, ImplicitStimulusEffect, NegativePosturalEffect, PerceptionEffect, PositivePosturalEffect, SomaticAccessEffect),
                                                GameComputation,
                                                Location (_locationActionManagement),
                                                SomaticAccessActionF (SomaticAccessActionF),
                                                updateActionConsequence)
import           Model.Parser.Atomics.Nouns    (Consumable (Consumable))
import           Model.Parser.Composites.Nouns (NounPhrase (SimpleNounPhrase))
import           Model.Parser.Composites.Verbs (ConsumptionVerbPhrase (ConsumptionVerbPhrase))
import           Relude.String                 (ToText (toText))

openEyesDenied :: SomaticAccessActionF
openEyesDenied = SomaticAccessActionF (const (const denied))
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
     youSeeM
     modifyNarration (updateActionConsequence msg)

msg :: Text
msg = "You open your eyes, and the world comes into focus."
