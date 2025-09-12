module ConstraintRefinement.Actions.Objects.Get.Constructors where
import           Control.Monad.Identity                           (Identity)
import qualified Data.Set
import           Debug.Trace                                      (trace)
import           GameState                                        (addToInventoryM,
                                                                   getObjectM,
                                                                   modifyNarration,
                                                                   modifySpatialRelationshipsForObjectM,
                                                                   updateActionConsequence)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs (get)
import           Model.Core                                       (AcquisitionActionF (CollectedF, LosesObjectF),
                                                                   ActionManagement (AVManagementKey),
                                                                   ActionManagementFunctions (ActionManagementFunctions),
                                                                   CoordinationResult (CoordinationResult, _actionEffectKeys, _computation),
                                                                   ActionEffectKey (AcquisitionalActionKey),
                                                                   Effect (ActionManagementEffect),
                                                                   GameComputation,
                                                                   Object (_objectActionManagement),
                                                                   SpatialRelationship (ContainedIn, Contains, SupportedBy, Supports))
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
        , _actionEffectKeys = map AcquisitionalActionKey getActionGIDs
        }

getFromSupportF :: GID Object -> AcquisitionActionF
getFromSupportF supportObjGID = LosesObjectF getit
  where
    getit :: GID Object
              -> GameComputation Identity CoordinationResult
    getit targetObjectGID = do
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
            -- Narration now comes from effect system

      pure $ CoordinationResult
        { _computation = computation
        , _actionEffectKeys = [] -- Container doesn't produce effects, only the collected object does
        }

