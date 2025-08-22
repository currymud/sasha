module Build.BedPuzzle.Actions.Get.Constructors where
import           Control.Monad.Identity                           (Identity)
import qualified Data.Map.Strict
import qualified Data.Set
import           Debug.Trace                                      (trace)
import           GameState                                        (addToInventoryM,
                                                                   getObjectM,
                                                                   modifyNarration,
                                                                   modifySpatialRelationshipsForObjectM)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs (get)
import           Model.GameState                                  (AcquisitionActionF (CollectedF, LosesObjectF),
                                                                   ActionKey (AcquisitionalActionKey),
                                                                   ActionManagement (AVManagementKey),
                                                                   ActionManagementFunctions (ActionManagementFunctions),
                                                                   CoordinationResult (CoordinationResult, _computation, _effectKeys),
                                                                   GameComputation,
                                                                   Object (_objectActionManagement),
                                                                   SpatialRelationship (ContainedIn, SupportedBy),
                                                                   updateActionConsequence)
import           Model.GID                                        (GID)


getObjectF :: GID Object -> AcquisitionActionF
getObjectF objectGID = CollectedF getit
  where
    getit :: Either (GameComputation Identity ()) (GameComputation Identity CoordinationResult)
    getit = Right $ do
      actionManagement <- _objectActionManagement <$> getObjectM objectGID
      let ActionManagementFunctions actionSet = actionManagement
      -- Find the single AVManagementKey entry that matches the 'get' verb
      let getActionGIDs = [gid | AVManagementKey verb gid <- Data.Set.toList actionSet, verb == get]
      pure $ CoordinationResult
        { _computation = addToInventoryM objectGID
        , _effectKeys = map AcquisitionalActionKey getActionGIDs
        }

getFromSupportF :: GID Object -> AcquisitionActionF
getFromSupportF supportObjGID = LosesObjectF getit
  where
    getit :: GID Object
              -> Either (GameComputation Identity ()) (GameComputation Identity CoordinationResult)
    getit targetObjectGID = Right $ do
      trace ("DEBUG: getFromSupportF executing with supportObjGID=" ++ show supportObjGID ++ " targetObjectGID=" ++ show targetObjectGID) $ pure ()

      -- Get the target object's action management
      actionManagement <- _objectActionManagement <$> getObjectM targetObjectGID
      let ActionManagementFunctions actionSet = actionManagement
      -- Find the AVManagementKey entry that matches the 'get' verb
      let getActionGIDs = [gid | AVManagementKey verb gid <- Data.Set.toList actionSet, verb == get]

      let computation = do
            -- Remove spatial relationships for this supporter
            modifySpatialRelationshipsForObjectM targetObjectGID $ \rels ->
              Data.Set.filter (\case
                SupportedBy oid -> oid /= supportObjGID
                ContainedIn oid -> oid /= supportObjGID
                _ -> True) rels
            -- Add to inventory
            addToInventoryM targetObjectGID
            -- Success narration
            modifyNarration $ updateActionConsequence "You pick it up."

      pure $ CoordinationResult
        { _computation = computation
        , _effectKeys = map AcquisitionalActionKey getActionGIDs
        }
