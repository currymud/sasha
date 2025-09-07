{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Open where
import           Control.Monad.Identity     (Identity)
import           Data.Set                   (Set)
import           Data.Text                  (Text)
import           GameState                  (modifyNarration)
import           GameState.ActionManagement (registerSystemEffect)
import           GameState.Perception       (youSeeM)
import           Model.Core            (ActionEffectKey (PlayerKey),
                                             ActionEffectMap (ActionEffectMap),
                                             GameComputation,
                                             SomaticAccessActionF (SomaticAccessActionF),
                                             SystemEffect (PerceptionSystemEffect),
                                             SystemEffectKey,
                                             SystemEffectRegistry,
                                             updateActionConsequence)

openEyesDenied :: SomaticAccessActionF
openEyesDenied = SomaticAccessActionF (const (const (const (const denied))))
 where
   denied :: GameComputation Identity ()
   denied = modifyNarration $ updateActionConsequence msg
   msg :: Text
   msg = "They're already open, relax."

openEyes :: SomaticAccessActionF
openEyes = SomaticAccessActionF opened
 where
   opened :: Set ActionEffectKey
             -> [SystemEffectKey]
             ->  ActionEffectMap
             ->  SystemEffectRegistry
             -> GameComputation Identity ()
   opened actionEffectKeys _ (ActionEffectMap actionEffectMap)_ = do
     modifyNarration (updateActionConsequence msg)
     youSeeM

msg :: Text
msg = "You open your eyes, and the world comes into focus."
