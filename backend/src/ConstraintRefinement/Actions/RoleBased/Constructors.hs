module ConstraintRefinement.Actions.RoleBased.Constructors where

import           Control.Monad.Except                             (MonadError (throwError))
import           Control.Monad.Identity                           (Identity)
import           Control.Monad.State                              (gets)
import qualified Data.Map.Strict
import           Data.Set                                         (Set)
import qualified Data.Set
import           GameState                                        (addToInventoryM,
                                                                   getObjectM,
                                                                   getPlayerLocationM,
                                                                   modifySpatialRelationshipsForObjectM)
import           GameState.ActionManagement                       (processEffectsFromRegistry)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs (get)
import           Model.Core                                       (ActionEffectKey (ContainerAcquisitionalActionKey, ObjectAcquisitionalActionKey),
                                                                   ActionManagement (ContainerAVManagementKey, ObjectAVManagementKey),
                                                                   ActionManagementFunctions (ActionManagementFunctions),
                                                                   AgentAcquisitionActionF (..),
                                                                   ContainerAcquisitionActionF (..),
                                                                   CoordinationResult (CoordinationResult, _actionEffectKeys, _computation),
                                                                   GameComputation,
                                                                   GameState (_world),
                                                                   Location (_locationInventory, _objectSemanticMap),
                                                                   Object (_objectActionManagement, _shortName),
                                                                   ObjectAcquisitionActionF (..),
                                                                   SpatialRelationship (ContainedIn, Contains, SupportedBy, Supports),
                                                                   SpatialRelationshipMap (SpatialRelationshipMap),
                                                                   World (_globalSemanticMap, _spatialRelationshipMap))
import           Model.GID                                        (GID)
import           Model.Parser.GCase                               (NounKey)


-- Helper functions from original Get.hs
validateObjectSearch :: NounKey -> GameComputation Identity (GID Object, GID Object)
validateObjectSearch nounKey = do
  maybeResult <- locationSearchStrategy nounKey
  case maybeResult of
    Nothing                        -> throwError "You don't see that here."
    Just (objectGID, containerGID) -> pure (objectGID, containerGID)

locationSearchStrategy :: NounKey -> GameComputation Identity (Maybe (GID Object, GID Object))
locationSearchStrategy targetNounKey = do
  world <- gets _world
  playerLocation <- getPlayerLocationM
  let globalSemanticMap = _globalSemanticMap world
      locationInventory = _locationInventory playerLocation
  case Data.Map.Strict.lookup targetNounKey globalSemanticMap of
    Just objSet | not (Data.Set.null objSet) -> do
      let availableObjects = Data.Set.filter (`Data.Set.member` locationInventory) objSet
      if not (Data.Set.null availableObjects)
        then do
          let targetGID = Data.Set.elemAt 0 availableObjects
          let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world
          case Data.Map.Strict.lookup targetGID spatialMap of
            Just relationships -> do
              let sources = getContainerSources relationships
              case sources of
                (sourceGID:_) -> pure $ Just (targetGID, sourceGID)
                []            -> pure Nothing
            Nothing -> pure Nothing
        else pure Nothing
    _ -> pure Nothing
  where
    getContainerSources :: Set SpatialRelationship -> [GID Object]
    getContainerSources relationships =
      [containerGID | ContainedIn containerGID <- Data.Set.toList relationships] ++
      [supporterGID | SupportedBy supporterGID <- Data.Set.toList relationships]

-- Role-based constructors that are type-safe
agentCannotAcquireF :: AgentAcquisitionActionF
agentCannotAcquireF = AgentCannotAcquireF processEffectsFromRegistry

objectCollectedF :: GID Object -> ObjectAcquisitionActionF
objectCollectedF objectGID = ObjectCollectedF getit
  where
    getit :: GameComputation Identity CoordinationResult
    getit = do
      actionManagement <- _objectActionManagement <$> getObjectM objectGID
      let ActionManagementFunctions actionSet = actionManagement
      let getActionGIDs = [gid | ObjectAVManagementKey verb gid <- Data.Set.toList actionSet, verb == get]
      pure $ CoordinationResult
        { _computation = addToInventoryM objectGID
        , _actionEffectKeys = map ObjectAcquisitionalActionKey getActionGIDs
        }

objectNotCollectableF :: ObjectAcquisitionActionF
objectNotCollectableF = ObjectNotCollectableF processEffectsFromRegistry

containerLosesObjectF :: GID Object -> ContainerAcquisitionActionF
containerLosesObjectF supportObjGID = ContainerLosesObjectF getit
  where
    getit :: GID Object -> GameComputation Identity CoordinationResult
    getit targetObjectGID = do
      actionManagement <- _objectActionManagement <$> getObjectM supportObjGID
      let ActionManagementFunctions actionSet = actionManagement
      let getActionGIDs = [gid | ContainerAVManagementKey verb gid <- Data.Set.toList actionSet, verb == get]
      let computation = do
            modifySpatialRelationshipsForObjectM targetObjectGID $ \rels ->
              Data.Set.filter (\case
                SupportedBy oid -> oid /= supportObjGID
                ContainedIn oid -> oid /= supportObjGID
                _ -> True) rels
            modifySpatialRelationshipsForObjectM supportObjGID $ \rels ->
              Data.Set.map (\case
                Supports objSet -> Supports (Data.Set.delete targetObjectGID objSet)
                Contains objSet -> Contains (Data.Set.delete targetObjectGID objSet)
                other -> other) rels
            addToInventoryM targetObjectGID
      pure $ CoordinationResult
        { _computation = computation
        , _actionEffectKeys = map ContainerAcquisitionalActionKey getActionGIDs
        }

containerCannotReleaseF :: ContainerAcquisitionActionF
containerCannotReleaseF = ContainerCannotReleaseF processEffectsFromRegistry
