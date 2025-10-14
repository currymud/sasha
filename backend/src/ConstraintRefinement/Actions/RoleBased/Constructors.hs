module ConstraintRefinement.Actions.RoleBased.Constructors where

import Control.Monad.Except (MonadError(throwError))
import Control.Monad.Identity (Identity)
import Control.Monad.State (gets)
import qualified Data.Map.Strict
import Data.Set (Set)
import qualified Data.Set
import Data.Text (pack)
import GameState (addToInventoryM, getObjectM, getPlayerLocationM, modifySpatialRelationshipsForObjectM)
import GameState.ActionManagement (processEffectsFromRegistry)
import Grammar.Parser.Partitions.Verbs.AcquisitionVerbs (get)
import Model.Core (AgentAcquisitionActionF(..), ObjectAcquisitionActionF(..), 
                   ContainerAcquisitionActionF(..), ActionEffectKey(ObjectAcquisitionalActionKey, ContainerAcquisitionalActionKey), 
                   ActionManagement(ObjectAVManagementKey, ContainerAVManagementKey), ActionManagementFunctions(ActionManagementFunctions), 
                   AcquisitionF, AcquisitionRes(..), SimpleAcquisitionRes(..), CompleteAcquisitionRes(..),
                   CoordinationResult(CoordinationResult, _actionEffectKeys, _computation),
                   GameComputation, GameState(_world), Location(_locationInventory, _objectSemanticMap),
                   Object(_objectActionManagement, _shortName), World(_globalSemanticMap, _spatialRelationshipMap),
                   SpatialRelationshipMap(SpatialRelationshipMap), SpatialRelationship(ContainedIn, Contains, SupportedBy, Supports))
import Model.GID (GID)
import Model.Parser.GCase (NounKey)

-- Agent-level constructor that coordinates role-based object and container actions
-- This eliminates programmer errors by using type-safe role-based coordination
agentGetF :: AgentAcquisitionActionF  
agentGetF = AgentAcquiresF getitRoleBased
  where
    getitRoleBased :: AcquisitionF
    getitRoleBased actionEffectKey ares = do
      case ares of
        Simple (SimpleAcquisitionRes {..}) -> do
          (oid, cid) <- validateObjectSearch _saObjectKey
          
          -- Use role-based object collection
          let ObjectCollectedF objectCollectionAction = objectCollectedF oid
          (CoordinationResult playerGetObjectF objectEffects) <- objectCollectionAction
          
          -- Use role-based container release  
          let ContainerLosesObjectF containerReleaseAction = containerLosesObjectF cid
          (CoordinationResult containerRemoveObjectF containerEffects) <- containerReleaseAction oid
          
          -- Coordinate both actions
          let allEffects = actionEffectKey:(objectEffects <> containerEffects)
          mapM_ processEffectsFromRegistry allEffects >> containerRemoveObjectF >> playerGetObjectF
          
        Complete (CompleteAcquisitionRes {..}) -> do
          -- Find both objects directly
          objectResult <- findObjectByKey _caObjectKey
          supportResult <- findObjectByKey _caSupportKey

          case (objectResult, supportResult) of
            (Nothing, _) -> throwError "You don't see that object here."
            (Just oid, Nothing) -> error ("programmer error: support object not found for" <> show oid)
            (Just oid, Just cid) -> do
              -- Validate the object is actually on/in the support
              world <- gets _world
              let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world
              case Data.Map.Strict.lookup oid spatialMap of
                Nothing -> error ("programmer error: " <> show oid <> " has no spatial relationships")
                Just relationships -> do
                  let isOnSupport = any (\case
                        SupportedBy sid -> sid == cid
                        ContainedIn cid'-> cid' == cid
                        _ -> False) (Data.Set.toList relationships)
                  if not isOnSupport
                    then do
                      objName <- _shortName <$> getObjectM oid
                      supportName <- _shortName <$> getObjectM cid
                      throwError $
                        "The " <> objName <>
                        " is not on the " <> supportName <> "."
                    else do
                      -- Use role-based coordination for Complete case too
                      let ObjectCollectedF objectCollectionAction = objectCollectedF oid
                      (CoordinationResult playerGetObjectF objectEffects) <- objectCollectionAction
                      
                      let ContainerLosesObjectF containerReleaseAction = containerLosesObjectF cid
                      (CoordinationResult containerRemoveObjectF containerEffects) <- containerReleaseAction oid
                      
                      let allEffects = actionEffectKey:(objectEffects <> containerEffects)
                      mapM_ processEffectsFromRegistry allEffects >> containerRemoveObjectF >> playerGetObjectF

      where
        findObjectByKey :: NounKey -> GameComputation Identity (Maybe (GID Object))
        findObjectByKey nounKey = do
          playerLocation <- getPlayerLocationM
          let objectSemanticMap = _objectSemanticMap playerLocation
          case Data.Map.Strict.lookup nounKey objectSemanticMap of
            Just objSet | not (Data.Set.null objSet) -> pure $ Just (Data.Set.elemAt 0 objSet)
            _ -> pure Nothing

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