module Build.BedPuzzle.Actions.Stand (standDenied, standUp) where
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.Identity    (Identity)
import qualified Data.Map.Strict
import           Data.Set                  (Set, filter, insert, toList)
import           Data.Text                 (Text, pack)
import           GameState                 (modifyLocationM, modifyNarration,
                                            processAcquisitionEffect)
import           GameState.Perception      (buildPerceptionMapFromObjects,
                                            computePerceivableObjects,
                                            modifyPerceptionMapM, youSeeM)
import           Model.GameState           (ActionEffectKey (LocationKey, ObjectKey, PlayerKey),
                                            ActionEffectMap (ActionEffectMap),
                                            ActionManagement (DSAManagementKey, ISAManagementKey, NPManagementKey, PPManagementKey, SSAManagementKey),
                                            ActionManagementFunctions (ActionManagementFunctions),
                                            GameComputation,
                                            Location (_locationActionManagement),
                                            PosturalActionF (PosturalActionF),
                                            updateActionConsequence)

standDenied :: PosturalActionF
standDenied = PosturalActionF (const (const denied))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You try to stand but the room starts spinning and you lay back down. There's some aspirin in your robe pocket."

standUp :: PosturalActionF
standUp = PosturalActionF stood
  where
    stood :: Set ActionEffectKey -> ActionEffectMap -> GameComputation Identity ()
    stood actionEffectKeys (ActionEffectMap actionEffectMap) = do
      -- Processsing ToDo
      modifyNarration (updateActionConsequence msg)

msg :: Text
msg = "You stand up, feeling more alert and ready for action."

standUpDenied :: PosturalActionF
standUpDenied = PosturalActionF (const (const denied))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You're already standing. No need to stand up again."
