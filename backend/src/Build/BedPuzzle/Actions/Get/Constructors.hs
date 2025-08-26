module Build.BedPuzzle.Actions.Get.Constructors where
import           Control.Monad.Identity                           (Identity)
import qualified Data.Set
import           Debug.Trace                                      (trace)
import           GameState                                        (addToInventoryM,
                                                                   getObjectM,
                                                                   modifyNarration,
                                                                   modifySpatialRelationshipsForObjectM)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs (get)
import           Model.GameState                                  (AcquisitionActionF (CollectedF, LosesObjectF),
                                                                   ActionManagement (AVManagementKey),
                                                                   ActionManagementFunctions (ActionManagementFunctions),
                                                                   CoordinationResult (CoordinationResult, _computation, _effectKeys, _fieldEffectKeys),
                                                                   EffectActionKey (AcquisitionalActionKey),
                                                                   GameComputation,
                                                                   Object (_objectActionManagement),
                                                                   SpatialRelationship (ContainedIn, Contains, SupportedBy, Supports),
                                                                   updateActionConsequence)
import           Model.GID                                        (GID)

getObjectF :: GID Object -> AcquisitionActionF
getObjectF objectGID = CollectedF getit
  where
    getit :: GameComputation Identity CoordinationResult
    getit = do
      actionManagement <- _objectActionManagement <$> getObjectM objectGID
      let ActionManagementFunctions actionSet = actionManagement
      -- Find the single AVManagementKey entry that matches the 'get' verb
      let getActionGIDs = [gid | AVManagementKey verb gid <- Data.Set.toList actionSet, verb == get]
      pure $ CoordinationResult
        { _computation = addToInventoryM objectGID
        , _effectKeys = map AcquisitionalActionKey getActionGIDs
        , _fieldEffectKeys = map AcquisitionalFieldEffectActionKey getActionGIDs
        }

getFromSupportF :: GID Object -> AcquisitionActionF
getFromSupportF supportObjGID = LosesObjectF getit
  where
    getit :: GID Object
              -> GameComputation Identity CoordinationResult
    getit targetObjectGID = do
      trace ("DEBUG: getFromSupportF executing with supportObjGID=" ++ show supportObjGID ++ " targetObjectGID=" ++ show targetObjectGID) $ pure ()

      -- Get the target object's action management
      actionManagement <- _objectActionManagement <$> getObjectM targetObjectGID
      let ActionManagementFunctions actionSet = actionManagement
      -- Find the AVManagementKey entry that matches the 'get' verb
      let getActionGIDs = [gid | AVManagementKey verb gid <- Data.Set.toList actionSet, verb == get]

      let computation = do
            -- Remove spatial relationships from target object (robe)
            modifySpatialRelationshipsForObjectM targetObjectGID $ \rels ->
              Data.Set.filter (\case
                SupportedBy oid -> oid /= supportObjGID
                ContainedIn oid -> oid /= supportObjGID
                _ -> True) rels

            -- Remove target object from support object's relationships (chair)
            modifySpatialRelationshipsForObjectM supportObjGID $ \rels ->
              Data.Set.map (\case
                Supports objSet -> Supports (Data.Set.delete targetObjectGID objSet)
                Contains objSet -> Contains (Data.Set.delete targetObjectGID objSet)
                other -> other) rels

            -- Add to inventory
            addToInventoryM targetObjectGID
            -- Success narration
            modifyNarration $ updateActionConsequence "You pick it up."

      pure $ CoordinationResult
        { _computation = computation
        , _effectKeys = map (RegularEffectKey . AcquisitionalActionKey) getActionGIDs
        , _fieldEffectKeys = map (FieldEffectKey . AcquisitionalFieldEffectActionKey) getActionGIDs
        }

