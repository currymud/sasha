module Build.BedPuzzle.Actions.Get.Constructors where
import           Control.Monad.Identity (Identity)
import           GameState              (addToInventoryM)
import           Model.GameState        (AcquisitionActionF (CollectedF, LosesObjectF),
                                         GameComputation, Object)
import           Model.GID              (GID)

getObjectF :: GID Object -> AcquisitionActionF
getObjectF objectGID  = CollectedF getit
  where
    getit :: Either (GameComputation Identity ()) (GameComputation Identity ())
    getit = Right $ addToInventoryM objectGID

getFromSupportF :: GID Object -> AcquisitionActionF
getFromSupportF _supportObjGID = LosesObjectF getit
  where
    getit :: GID Object
              -> Either (GameComputation Identity ()) (GameComputation Identity ())
    getit _supportedObjectGID = do
-- Step 1: If the contained object is not in the robes spatial relation , then it's  Left modifyNarration "That's not in the robe."
-- Step 2: If it is, then change the spatial relationship of the contained object to be in the player's inventory.
--  this is how you do narrations  modifyNarration $ updateActionConsequence msg
      Left $ pure ()
