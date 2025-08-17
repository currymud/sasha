module Build.BedPuzzle.Actions.Get.Constructors where
import           Control.Monad.Identity (Identity)
import qualified Data.Set
import           Debug.Trace            (trace)
import           GameState              (addToInventoryM, modifyNarration,
                                         modifySpatialRelationshipsForObjectM)
import           Model.GameState        (AcquisitionActionF (CollectedF, LosesObjectF),
                                         GameComputation, Object,
                                         SpatialRelationship (ContainedIn, SupportedBy),
                                         updateActionConsequence)
import           Model.GID              (GID)

getObjectF :: GID Object -> AcquisitionActionF
getObjectF objectGID  = CollectedF getit
  where
    getit :: Either (GameComputation Identity ()) (GameComputation Identity ())
    getit = Right $ do
      addToInventoryM objectGID

getFromSupportF :: GID Object -> AcquisitionActionF
getFromSupportF supportObjGID = LosesObjectF getit
  where
    getit :: GID Object
              -> Either (GameComputation Identity ()) (GameComputation Identity ())
    getit targetObjectGID = Right $ do
      trace ("DEBUG: getFromSupportF executing with supportObjGID=" ++ show supportObjGID ++ " targetObjectGID=" ++ show targetObjectGID) $ pure ()
      -- Remove both SupportedBy and ContainedIn relationships for this supporter
      modifySpatialRelationshipsForObjectM targetObjectGID $ \rels ->
        Data.Set.filter (\case
          SupportedBy oid -> oid /= supportObjGID
          ContainedIn oid -> oid /= supportObjGID
          _ -> True) rels
      -- Add to inventory
      addToInventoryM targetObjectGID
      -- Success narration
      trace "DEBUG: About to set 'You pick it up.' narration" $ pure ()
      modifyNarration $ updateActionConsequence "You pick it up."
      trace "DEBUG: Set 'You pick it up.' narration" $ pure ()

        {-
getFromSupportF :: GID Object -> AcquisitionActionF
getFromSupportF supportObjGID = LosesObjectF getit
  where
    getit :: GID Object
              -> Either (GameComputation Identity ()) (GameComputation Identity ())
    getit targetObjectGID = Right $ do
      trace ("DEBUG: getFromSupportF called with supportObjGID=" ++ show supportObjGID ++ " targetObjectGID=" ++ show targetObjectGID) $ pure ()
      -- Remove both SupportedBy and ContainedIn relationships for this supporter
      modifySpatialRelationshipsForObjectM targetObjectGID $ \rels ->
        Data.Set.filter (\case
          SupportedBy oid -> oid /= supportObjGID
          ContainedIn oid -> oid /= supportObjGID
          _ -> True) rels
      -- Add to inventory
      addToInventoryM targetObjectGID
      -- Success narration
      modifyNarration $ updateActionConsequence "You pick it up."
      -}
