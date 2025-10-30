module ActionDiscovery.Manipulate.ContainerAccess.Open where
import           Control.Monad.Error.Class     (MonadError (throwError))
import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader          (asks)
import qualified Data.Map.Strict
import qualified Data.Set
import           GameState                     (getObjectM, getPlayerLocationM,
                                                getPlayerM,
                                                parseContainerAccessPhrase)
import           GameState.ActionManagement    (lookupAgentContainerAccessVerbPhrase,
                                                lookupInstrumentContainerAccessVerbPhrase,
                                                lookupLocationContainerAccessVerbPhrase,
                                                lookupObjectContainerAccessVerbPhrase)
import           Model.Core                    (AccessRes (CompleteAR, SimpleAR),
                                                ActionEffectKey (AgentContainerAccessActionKey, InstrumentContainerAccessActionKey, LocationContainerAccessActionKey, ObjectContainerAccessActionKey),
                                                ActionManagementFunctions,
                                                ActionMaps (_agentContainerAccessActionMap, _instrumentContainerAccessActionMap, _locationContainerAccessActionMap, _objectContainerAccessActionMap),
                                                AgentContainerAccessActionF (AgentCanAccessF, AgentCannotAccessF),
                                                CompleteAccessRes (CompleteAccessRes),
                                                InstrumentContainerAccessActionF (InstrumentCanAccessF, InstrumentCannotAccessF),
                                                Config (_actionMaps),
                                                GameComputation,
                                                Location (_locationActionManagement, _objectSemanticMap),
                                                LocationContainerAccessActionF (LocationCanAccessContainerF, LocationCannotAccessContainerF),
                                                Object (_objectActionManagement),
                                                ObjectContainerAccessActionF (ContainingObjectCanAccessF, ContainingObjectCannotAccessF),
                                                Player (_playerActions),
                                                SimpleAccessRes (SimpleAccessRes, _saContainerKey, _saContainerPhrase),
                                                SimpleAccessSearchStrategy)
import           Model.GID                     (GID)
import           Model.Parser.Composites.Verbs (ContainerAccessVerbPhrase)
import           Model.Parser.GCase            (NounKey)

-- Preferred role-based implementation using Agent, Location, and Object specific actions
manageContainerAccessProcess :: ContainerAccessVerbPhrase
                                  -> GameComputation Identity ()
manageContainerAccessProcess cavp = case caRes of
  SimpleAR (SimpleAccessRes {..}) -> do
    oid <- validateObjectSearch objectSearchStrategy _saContainerKey
    containerAvailableActions <- _objectActionManagement <$> getObjectM oid
    agentAvailableActions  <- _playerActions <$> getPlayerM
    locationsAvailableActions <- _locationActionManagement <$>  getPlayerLocationM
    let lookupAgentActionGID = lookupAgentActionF agentAvailableActions
        lookupLocationActionGID = lookupLocationActionF locationsAvailableActions
        lookupObjectContainerAccessActionGID = lookupObjectContainerAccessActionF containerAvailableActions
    case (lookupAgentActionGID, lookupLocationActionGID, lookupObjectContainerAccessActionGID) of
      (Nothing, Nothing, Nothing) -> throwError "This cannot be opened."
      (Just agentActionGID,Just locationActionGID, Just containerActionGID) -> do
        agentActionMap <- asks (_agentContainerAccessActionMap . _actionMaps)
        locationActionMap <- asks (_locationContainerAccessActionMap . _actionMaps)
        containerActionMap <- asks (_objectContainerAccessActionMap . _actionMaps)
        let agentActionEffectKey = AgentContainerAccessActionKey agentActionGID
            containerActionEffectKey = ObjectContainerAccessActionKey containerActionGID
            locationActionEffectKey = LocationContainerAccessActionKey locationActionGID
            lookupAgentAction = Data.Map.Strict.lookup agentActionGID agentActionMap
            lookupLocationAction = Data.Map.Strict.lookup locationActionGID locationActionMap
            lookupContainerAction = Data.Map.Strict.lookup containerActionGID containerActionMap

        case (lookupAgentAction,lookupLocationAction,lookupContainerAction) of
          (Nothing,_,_) -> error $ "Programmer Error: No agent container access action found for GID: " ++ show agentActionGID
          (_,Nothing,_) -> error $ "Programmer Error: No location container access action found for GID: " ++ show locationActionGID
          (_,_,Nothing) -> error $ "Programmer Error: No object container access action found for GID: " ++ show containerActionGID

          (Just (AgentCannotAccessF agentActionF),_,_) -> agentActionF agentActionEffectKey
          (Just (AgentCanAccessF agentActionF),Just (LocationCannotAccessContainerF locationActionF),_) ->
            agentActionF agentActionEffectKey >> locationActionF locationActionEffectKey
          (Just (AgentCanAccessF agentActionF), _, Just (ContainingObjectCannotAccessF containingObjectActionF)) ->
            agentActionF agentActionEffectKey >> containingObjectActionF containerActionEffectKey
          (Just (AgentCanAccessF agentActionF), Just (LocationCanAccessContainerF locationActionF), Just (ContainingObjectCanAccessF containingObjectActionF)) ->
            agentActionF agentActionEffectKey
              >> locationActionF locationActionEffectKey
              >> containingObjectActionF containerActionEffectKey
      (_,_,_) -> throwError "This cannot be opened."
  CompleteAR (CompleteAccessRes {..}) -> error "openF: Complete Access Result not implemented."
  where
    caRes :: AccessRes
    caRes = parseContainerAccessPhrase cavp
    lookupAgentActionF :: ActionManagementFunctions -> Maybe (GID AgentContainerAccessActionF)
    lookupAgentActionF = lookupAgentContainerAccessVerbPhrase cavp
    lookupInstrumentActionF :: ActionManagementFunctions -> Maybe (GID InstrumentContainerAccessActionF)
    lookupInstrumentActionF = lookupInstrumentContainerAccessVerbPhrase cavp
    lookupLocationActionF :: ActionManagementFunctions -> Maybe (GID LocationContainerAccessActionF)
    lookupLocationActionF = lookupLocationContainerAccessVerbPhrase cavp
    lookupObjectContainerAccessActionF :: ActionManagementFunctions -> Maybe (GID ObjectContainerAccessActionF)
    lookupObjectContainerAccessActionF = lookupObjectContainerAccessVerbPhrase cavp

-- | ToDo Clarification system. object identification assumes only one object in location matches nounkey.
objectSearchStrategy :: SimpleAccessSearchStrategy
objectSearchStrategy nounkey = do
  objectSemanticMap <- _objectSemanticMap <$> getPlayerLocationM
  case Data.Map.Strict.lookup nounkey objectSemanticMap of
    Just objSet
      | not (Data.Set.null objSet) -> pure $ Just (Data.Set.elemAt 0 objSet)
    _ -> pure Nothing

validateObjectSearch :: SimpleAccessSearchStrategy
                          -> NounKey
                          -> GameComputation Identity (GID Object)
validateObjectSearch searchStrategy nounKey = do
  maybeResult <- searchStrategy nounKey
  case maybeResult of
    Nothing        -> throwError "You don't see that here."
    Just objectGID -> pure objectGID
