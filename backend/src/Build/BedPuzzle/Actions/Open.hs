{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Open where
import           Control.Monad.Identity (Identity)
import           Data.Set               (Set)
import           Data.Text              (Text)
import           GameState              (modifyNarration)
import           Model.GameState        (ActionEffectKey (PlayerKey),
                                         ActionEffectMap (ActionEffectMap),
                                         GameComputation,
                                         SomaticAccessActionF (SomaticAccessActionF),
                                         updateActionConsequence)

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
     modifyNarration (updateActionConsequence msg)

msg :: Text
msg = "You open your eyes, and the world comes into focus."
