module ActionDiscovery.Get.Acquisition.Get (manageAcquisitionProcess) where

import           Control.Applicative                              ((<|>))
import           Control.Monad                                    (join)
import           Control.Monad.Error.Class                        (throwError)
import           Control.Monad.Identity                           (Identity)
import           Control.Monad.Reader.Class                       (asks)
import           Control.Monad.State                              (gets)
import qualified Data.Map.Strict
import qualified Data.Maybe
import           Data.Set                                         (Set, elemAt,
                                                                   filter,
                                                                   member, null,
                                                                   toList)
import qualified Data.Text
import           Data.Void                                        (Void)
import           GameState                                        (getObjectM,
                                                                   getPlayerLocationM,
                                                                   getPlayerM,
                                                                   parseAcquisitionPhrase)
import           GameState.ActionManagement                       (findAgentAAKey,
                                                                   findAgentAVKey,
                                                                   findContainerAVKey,
                                                                   findObjectAVKey,
                                                                   processEffectsFromRegistry)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs (get)
import           Model.Core                                       (AcquisitionRes (Complete, Simple),
                                                                   ActionEffectKey (AgentAcquisitionalActionKey, ContainerAcquisitionalActionKey, ObjectAcquisitionalActionKey),
                                                                   ActionMaps (_agentAcquisitionActionMap, _containerAcquisitionActionMap, _objectAcquisitionActionMap),
                                                                   AgentAcquisitionActionF (_unAAA),
                                                                   AgentAcquisitionActionMap,
                                                                   CompleteAcquisitionRes (CompleteAcquisitionRes),
                                                                   Config (_actionMaps),
                                                                   ContainerAcquisitionActionF (_unCFA),
                                                                   ContainerAcquisitionActionMap,
                                                                   CoordinationResult (CoordinationResult),
                                                                   GameComputation,
                                                                   GameState (_world),
                                                                   Location (_locationInventory, _objectSemanticMap),
                                                                   Object (_objectActionManagement),
                                                                   ObjectAcquisitionActionF (_unOAA),
                                                                   ObjectAcquisitionActionMap,
                                                                   Player (_playerActions),
                                                                   SearchStrategy,
                                                                   SimpleAcquisitionRes (SimpleAcquisitionRes),
                                                                   SpatialRelationship (ContainedIn, SupportedBy),
                                                                   SpatialRelationshipMap (SpatialRelationshipMap),
                                                                   World (_globalSemanticMap, _spatialRelationshipMap),
                                                                   _saObjectKey)
import           Model.GID                                        (GID)
import           Model.Parser.Composites.Verbs                    (AcquisitionVerbPhrase)
import           Model.Parser.GCase                               (NounKey)

-- ToDo: Add Location related values,
-- Location effects need to be included in the process
--    arRes = parseAcquisitionPhrase avp

manageAcquisitionProcess :: AcquisitionVerbPhrase -> GameComputation Identity ()
manageAcquisitionProcess avp = do
  availableActions <- _playerActions <$> getPlayerM
  agentActionMap <- asks ( _agentAcquisitionActionMap . _actionMaps)
  let agentGID = findAgentGID availableActions
      agentEffectKey = AgentAcquisitionalActionKey agentGID
      acquisitionF = _unAAA $ Data.Maybe.fromMaybe agentActionErr $ Data.Map.Strict.lookup agentGID agentActionMap
  case arRes of
        Simple (SimpleAcquisitionRes {..}) -> do
          (oid, cid) <- validateObjectSearch _saObjectKey
          -- Get role-specific action maps
          objActionMap <- getObjectAcquisitionActionMap
          conActionMap <- getContainerAcquisitionActionMap

          -- Get action management for lookups
          objActionManagement <- _objectActionManagement <$> getObjectM oid
          conActionManagement <- _objectActionManagement <$> getObjectM cid

          -- Find role-specific action GIDs
          case (findObjectAVKey get objActionManagement, findContainerAVKey get conActionManagement) of
            (Nothing, _) -> error $ "Object " <> show oid <> " does not have object acquisition action."
            (_, Nothing) -> error $ "Container " <> show cid <> " does not have container acquisition action."
            (Just oKey, Just cKey) -> do
              let objEffectKey = ObjectAcquisitionalActionKey oKey
                  containerEffectKey = ContainerAcquisitionalActionKey cKey

              -- Lookup role-specific actions
              objectActionF <- _unOAA <$> lookupObjectAcquisitionAction objActionMap oid
              containerActionF <- _unCFA <$> lookupContainerAcquisitionAction conActionMap cid
              acquisitionF agentEffectKey >> objectActionF objEffectKey >> containerActionF containerEffectKey
        Complete (CompleteAcquisitionRes {..}) -> do
          pure ()
{-
          -- Find both objects directly
          objectResult <- findObjectByKey _caObjectKey
          supportResult <- findObjectByKey _caSupportKey
          case (objectResult, supportResult) of
            (Nothing, _) -> throwError "You don't see that object here."
            (Just oid, Nothing) -> error ("programer error: support object not found for" <> show oid)
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
                      -- Get role-specific action maps
                      objActionMap <- getObjectAcquisitionActionMap
                      conActionMap <- getContainerAcquisitionActionMap
                      -- Get action management for lookups
                      objActionManagement <- _objectActionManagement <$> getObjectM oid
                      conActionManagement <- _objectActionManagement <$> getObjectM cid
                      -- Find role-specific action GIDs
                      case (findObjectAVKey get objActionManagement, findContainerAVKey get conActionManagement) of
                        (Nothing, _) -> error $ "Object " <> show oid <> " does not have object acquisition action."
                        (_, Nothing) -> error $ "Container " <> show cid <> " does not have container acquisition action."
                        (Just oKey, Just cKey) -> do
                          let objEffectKey = ObjectAcquisitionalActionKey oKey
                              containerEffectKey = ContainerAcquisitionalActionKey cKey
                          -- Lookup role-specific actions
                          objectActionF <- _unOAA $ lookupObjectAcquisitionAction objActionMap oid
                          containerActionF <- _unCFA $ lookupContainerAcquisitionAction conActionMap cid
                          acquisitionF actionEffectKey >> objectActionF objEffectKey >> containerActionF containerEffectKey
                          -}
  where
    arRes = parseAcquisitionPhrase avp
    agentIdError :: GID AgentAcquisitionActionF
    agentIdError = error "No agent acquisition action available for this action."
    findAgentGID availableActions = Data.Maybe.fromMaybe agentIdError $ f availableActions

    findObjectByKey :: NounKey -> GameComputation Identity (Maybe (GID Object))
    findObjectByKey nounKey = do
        playerLocation <- getPlayerLocationM
        let objectSemanticMap = _objectSemanticMap playerLocation
        case Data.Map.Strict.lookup nounKey objectSemanticMap of
          Just objSet | not (Data.Set.null objSet) -> pure $ Just (Data.Set.elemAt 0 objSet)
          _ -> pure Nothing
    f availableActions =
        findAgentAAKey avp availableActions
          <|> findAgentAVKey get availableActions

validateObjectSearch :: NounKey
                          -> GameComputation Identity (GID Object, GID Object)
validateObjectSearch nounKey = do
  maybeResult <- locationSearchStrategy nounKey
  case maybeResult of
    Nothing                        -> throwError "You don't see that here."
    Just (objectGID, containerGID) -> pure (objectGID, containerGID)

lookupObjectAcquisitionAction :: ObjectAcquisitionActionMap
                             -> GID Object
                             -> GameComputation Identity ObjectAcquisitionActionF
lookupObjectAcquisitionAction actionMap oid = do
  actionMgmt <- _objectActionManagement <$> getObjectM oid
  case findObjectAVKey get actionMgmt of
    Nothing -> throwError $ (Data.Text.pack . show) oid <> " does not have a 'get' action."
    Just actionGID -> do
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> throwError $ "No object acquisition action found for GID: " <> (Data.Text.pack . show) actionGID
        Just action -> pure action

lookupContainerAcquisitionAction :: ContainerAcquisitionActionMap
                                -> GID Object
                                -> GameComputation Identity ContainerAcquisitionActionF
lookupContainerAcquisitionAction actionMap cid = do
  actionMgmt <- _objectActionManagement <$> getObjectM cid
  case findContainerAVKey get actionMgmt of
    Nothing -> throwError $ (Data.Text.pack . show) cid <> " does not have a container 'get' action."
    Just actionGID -> do
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> throwError $ "No container acquisition action found for GID: " <> (Data.Text.pack . show) actionGID
        Just action -> pure action

getObjectAcquisitionActionMap :: GameComputation Identity ObjectAcquisitionActionMap
getObjectAcquisitionActionMap = asks (_objectAcquisitionActionMap . _actionMaps)

getContainerAcquisitionActionMap :: GameComputation Identity ContainerAcquisitionActionMap
getContainerAcquisitionActionMap = asks (_containerAcquisitionActionMap . _actionMaps)

locationSearchStrategy :: SearchStrategy
locationSearchStrategy targetNounKey = do
  world <- gets _world
  playerLocation <- getPlayerLocationM
  let globalSemanticMap = _globalSemanticMap world
      locationInventory = _locationInventory playerLocation
  case Data.Map.Strict.lookup targetNounKey globalSemanticMap of
    Just objSet | not (Data.Set.null objSet) -> do
      -- Find first object that's in the current location's inventory
      let availableObjects = Data.Set.filter (`Data.Set.member` locationInventory) objSet
      if not (Data.Set.null availableObjects)
        then do
          let targetGID = Data.Set.elemAt 0 availableObjects
          -- Find what contains/supports this object
          let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world
          case Data.Map.Strict.lookup targetGID spatialMap of
            Just relationships -> do
              let sources = getContainerSources relationships
              case sources of
                (sourceGID:_) -> pure $ Just (targetGID, sourceGID)
                []            -> pure Nothing  -- Object exists but has no container
            Nothing -> pure Nothing
        else pure Nothing  -- Object exists but not in this location
    _ -> pure Nothing
  where
    getContainerSources :: Set SpatialRelationship -> [GID Object]
    getContainerSources relationships =
      [containerGID | ContainedIn containerGID <- Data.Set.toList relationships] ++
      [supporterGID | SupportedBy supporterGID <- Data.Set.toList relationships]
