module GameState.ActionManagement where
import           Control.Monad                 (unless, when)
import           Control.Monad.Identity        (Identity)
import           Control.Monad.State.Strict    (gets, modify')
import qualified Data.Foldable
import qualified Data.Map.Strict
import           Data.Maybe                    (listToMaybe)
import           Data.Set                      (Set)
import qualified Data.Set
import           Data.Text                     (intercalate)
import           GameState                     (getObjectM,
                                                getTopLevelInventoryObjectsM,
                                                modifyLocationM,
                                                modifyNarration, modifyObjectM,
                                                modifyPlayerM,
                                                updateActionConsequence)
import           GameState.EffectRegistry      (lookupActionEffectsInRegistry)
import           GameState.Perception          (youSeeM)
import           Model.Core                    (ActionEffectKey,
                                                ActionEffectMap (ActionEffectMap),
                                                ActionManagement (AgentAAManagementKey, AgentAVManagementKey, AgentDSAContainerManagementKey, AgentDSAManagementKey, AgentISAManagementKey, CAManagementKey, CONManagementKey, ContainerAAManagementKey, ContainerAVManagementKey, ContainerDSAContainerManagementKey, LocationAAManagementKey, LocationAVManagementKey, LocationDSAContainerManagementKey, LocationDSAManagementKey, LocationISAManagementKey, NPManagementKey, ObjectAAManagementKey, ObjectAVManagementKey, ObjectDSAManagementKey, PPManagementKey, SAConManagementKey, SSAManagementKey),
                                                ActionManagementFunctions (ActionManagementFunctions),
                                                ActionManagementOperation (AddAgentAcquisitionVerb, AddAgentAcquisitionVerbPhrase, AddAgentDirectionalContainerStimulus, AddAgentDirectionalStimulus, AddAgentImplicitStimulus, AddConsumption, AddContainerAccess, AddContainerAccessVerb, AddContainerAcquisitionVerb, AddContainerAcquisitionVerbPhrase, AddContainerDirectionalContainerStimulus, AddLocationAcquisitionVerb, AddLocationAcquisitionVerbPhrase, AddLocationDirectionalContainerStimulus, AddLocationDirectionalStimulus, AddLocationImplicitStimulus, AddNegativePostural, AddObjectAcquisitionVerb, AddObjectAcquisitionVerbPhrase, AddObjectDirectionalStimulus, AddPositivePostural, AddSomaticAccess),
                                                AgentAcquisitionActionF,
                                                AgentDirectionalStimulusActionF,
                                                AgentDirectionalStimulusContainerActionF,
                                                AgentImplicitStimulusActionF,
                                                ConsumptionActionF,
                                                ContainerAccessActionF,
                                                ContainerAcquisitionActionF,
                                                ContainerDirectionalStimulusContainerActionF,
                                                Effect (ActionManagementEffect, FieldUpdateEffect, NarrationEffect),
                                                FieldUpdateOperation (LocationTitle, ObjectDescription, ObjectShortName, PlayerLocation),
                                                GameComputation,
                                                GameState (_player, _systemEffectRegistry, _world),
                                                Location (_locationActionManagement),
                                                LocationAcquisitionActionF,
                                                LocationDirectionalStimulusActionF,
                                                LocationDirectionalStimulusContainerActionF,
                                                LocationImplicitStimulusActionF,
                                                NarrationComputation (InventoryNarration, LookAtNarration, LookInNarration, LookNarration, StaticNarration),
                                                Object (_description, _objectActionManagement, _shortName),
                                                ObjectAcquisitionActionF,
                                                ObjectDirectionalStimulusActionF,
                                                Player (_location, _playerActions),
                                                PlayerKey (PlayerKeyLocation, PlayerKeyObject),
                                                PosturalActionF,
                                                SomaticAccessActionF,
                                                SpatialRelationship (ContainedIn, Contains, Inventory, SupportedBy, Supports),
                                                SpatialRelationshipMap (SpatialRelationshipMap),
                                                SystemEffect,
                                                SystemEffectConfig,
                                                SystemEffectKey,
                                                TargetEffectKey (LocationKey, ObjectKey, PlayerKey),
                                                World (_spatialRelationshipMap),
                                                _title)
import           Model.GID                     (GID)
import           Model.Parser.Atomics.Verbs    (AcquisitionVerb,
                                                ConsumptionVerb,
                                                DirectionalStimulusVerb,
                                                ImplicitStimulusVerb,
                                                NegativePosturalVerb,
                                                PositivePosturalVerb,
                                                SimpleAccessVerb,
                                                SomaticAccessVerb)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase (AcquisitionVerbPhrase, SimpleAcquisitionVerbPhrase),
                                                ContainerAccessVerbPhrase,
                                                PosturalVerbPhrase (NegativePosturalVerbPhrase, PositivePosturalVerbPhrase))

modifyPlayerActionManagementM :: (ActionManagementFunctions -> ActionManagementFunctions)
                              -> GameComputation Identity ()
modifyPlayerActionManagementM actionF = do
  modify' $ \gs ->
    gs { _player = (_player gs) { _playerActions = actionF (_playerActions (_player gs)) } }

modifyLocationActionManagementM :: GID Location
                                -> (ActionManagementFunctions -> ActionManagementFunctions)
                                -> GameComputation Identity ()
modifyLocationActionManagementM lid actionF = do
  modifyLocationM lid $ \loc ->
    loc { _locationActionManagement = actionF (_locationActionManagement loc) }

registerSystemEffect :: SystemEffectKey
                          -> GID SystemEffect
                          -> SystemEffectConfig
                          -> GameComputation Identity ()
registerSystemEffect key effectGID config = modify' $ \gs ->
  let currentRegistry = _systemEffectRegistry gs
      newEffectMap = Data.Map.Strict.singleton effectGID config
      updatedRegistry = Data.Map.Strict.insertWith Data.Map.Strict.union key newEffectMap currentRegistry
  in gs { _systemEffectRegistry = updatedRegistry }

removeSystemEffect :: SystemEffectKey -> GID SystemEffect -> GameComputation Identity ()
removeSystemEffect key effectGID = modify' $ \gs ->
  let currentRegistry = _systemEffectRegistry gs
      updatedRegistry = Data.Map.Strict.adjust (Data.Map.Strict.delete effectGID) key currentRegistry
  in gs { _systemEffectRegistry = updatedRegistry }

processEffectsFromRegistry :: ActionEffectKey -> GameComputation Identity ()
processEffectsFromRegistry actionKey = do
  maybeEffectMap <- lookupActionEffectsInRegistry actionKey
  Data.Foldable.for_ maybeEffectMap processAllEffects

modifyObjectActionManagementM :: GID Object
                             -> (ActionManagementFunctions -> ActionManagementFunctions)
                             -> GameComputation Identity ()
modifyObjectActionManagementM oid actionF = do
 modifyObjectM oid $ \obj ->
   obj { _objectActionManagement = actionF (_objectActionManagement obj) }

processAllEffects :: ActionEffectMap -> GameComputation Identity ()
processAllEffects (ActionEffectMap effectMap) = do
  mapM_ processEffectEntry (Data.Map.Strict.toList effectMap)
  where
    processEffectEntry :: (TargetEffectKey, Set Effect) -> GameComputation Identity ()
    processEffectEntry (effectKey, effects) = do
      mapM_ (processEffect effectKey) (Data.Set.toList effects)

processEffect :: TargetEffectKey -> Effect -> GameComputation Identity ()
processEffect (LocationKey lid) (ActionManagementEffect (AddContainerAccessVerb verb newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case SAConManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (SAConManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (ActionManagementEffect (AddContainerAccess cavp newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case CONManagementKey p _ -> p /= cavp; _ -> True) actionSet
        updatedActions = Data.Set.insert (CONManagementKey cavp newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (ActionManagementEffect (AddAgentImplicitStimulus verb newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AgentISAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (AgentISAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (ActionManagementEffect (AddLocationImplicitStimulus verb newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case LocationISAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (LocationISAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (ActionManagementEffect (AddSomaticAccess verb newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case SSAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (SSAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (ActionManagementEffect (AddConsumption verb targetOid newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case CAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (CAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (ActionManagementEffect (AddPositivePostural verb newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case PPManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (PPManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (ActionManagementEffect (AddNegativePostural verb newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case NPManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (NPManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey _) (FieldUpdateEffect (ObjectShortName targetOid newShortName)) = do
  modifyObjectM targetOid $ \obj -> obj { _shortName = newShortName }

processEffect (LocationKey _) (FieldUpdateEffect (ObjectDescription targetOid newDescription)) = do
  modifyObjectM targetOid $ \obj -> obj { _description = newDescription }

processEffect (LocationKey _) (FieldUpdateEffect (LocationTitle targetLid newTitle)) = do
  modifyLocationM targetLid $ \loc -> loc { _title = newTitle }

processEffect (LocationKey _) (FieldUpdateEffect (PlayerLocation newLocationGID)) = do
  modifyPlayerM $ \player -> player { _location = newLocationGID }

processEffect (ObjectKey oid) (ActionManagementEffect (AddContainerAccessVerb verb newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case SAConManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (SAConManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ActionManagementEffect (AddContainerAccess cavp newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case CONManagementKey p _ -> p /= cavp; _ -> True) actionSet
        updatedActions = Data.Set.insert (CONManagementKey cavp newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ActionManagementEffect (AddAgentImplicitStimulus verb newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AgentISAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (AgentISAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ActionManagementEffect (AddLocationImplicitStimulus verb newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case LocationISAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (LocationISAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ActionManagementEffect (AddSomaticAccess verb newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case SSAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (SSAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ActionManagementEffect (AddConsumption verb targetOid newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case CAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (CAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ActionManagementEffect (AddPositivePostural verb newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case PPManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (PPManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ActionManagementEffect (AddNegativePostural verb newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case NPManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (NPManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey _) (FieldUpdateEffect (ObjectShortName targetOid newShortName)) = do
  modifyObjectM targetOid $ \obj -> obj { _shortName = newShortName }

processEffect (ObjectKey _) (FieldUpdateEffect (ObjectDescription targetOid newDescription)) = do
  modifyObjectM targetOid $ \obj -> obj { _description = newDescription }

processEffect (ObjectKey _) (FieldUpdateEffect (LocationTitle targetLid newTitle)) = do
  modifyLocationM targetLid $ \loc -> loc { _title = newTitle }

processEffect (ObjectKey _) (FieldUpdateEffect (PlayerLocation newLocationGID)) = do
  modifyPlayerM $ \player -> player { _location = newLocationGID }

processEffect (PlayerKey (PlayerKeyLocation lid)) (ActionManagementEffect (AddContainerAccessVerb verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case SAConManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (SAConManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (ActionManagementEffect (AddContainerAccessVerb verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case SAConManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (SAConManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyLocation lid)) (ActionManagementEffect (AddContainerAccess cavp newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case CONManagementKey p _ -> p /= cavp; _ -> True) actionSet
        updatedActions = Data.Set.insert (CONManagementKey cavp newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (ActionManagementEffect (AddContainerAccess cavp newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case CONManagementKey p _ -> p /= cavp; _ -> True) actionSet
        updatedActions = Data.Set.insert (CONManagementKey cavp newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyLocation lid)) (ActionManagementEffect (AddAgentImplicitStimulus verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AgentISAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (AgentISAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (ActionManagementEffect (AddAgentImplicitStimulus verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AgentISAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (AgentISAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyLocation lid)) (ActionManagementEffect (AddLocationImplicitStimulus verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case LocationISAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (LocationISAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (ActionManagementEffect (AddLocationImplicitStimulus verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case LocationISAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (LocationISAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyLocation lid)) (ActionManagementEffect (AddSomaticAccess verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case SSAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (SSAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (ActionManagementEffect (AddSomaticAccess verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case SSAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (SSAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey _) (ActionManagementEffect (AddAgentAcquisitionVerb verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AgentAVManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (AgentAVManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey _) (ActionManagementEffect (AddAgentAcquisitionVerbPhrase phrase newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AgentAAManagementKey p _ -> p /= phrase; _ -> True) actionSet
        updatedActions = Data.Set.insert (AgentAAManagementKey phrase newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyLocation lid)) (ActionManagementEffect (AddAgentDirectionalStimulus verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AgentDSAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (AgentDSAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (ActionManagementEffect (AddAgentDirectionalStimulus verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AgentDSAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (AgentDSAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ActionManagementEffect (AddObjectAcquisitionVerb verb newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ObjectAVManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (ObjectAVManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ActionManagementEffect (AddObjectAcquisitionVerbPhrase phrase newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ObjectAAManagementKey p _ -> p /= phrase; _ -> True) actionSet
        updatedActions = Data.Set.insert (ObjectAAManagementKey phrase newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ActionManagementEffect (AddContainerAcquisitionVerb verb newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ContainerAVManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (ContainerAVManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ActionManagementEffect (AddContainerAcquisitionVerbPhrase phrase newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ContainerAAManagementKey p _ -> p /= phrase; _ -> True) actionSet
        updatedActions = Data.Set.insert (ContainerAAManagementKey phrase newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ActionManagementEffect (AddAgentDirectionalStimulus verb newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AgentDSAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (AgentDSAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ActionManagementEffect (AddLocationDirectionalStimulus verb newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case LocationDSAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (LocationDSAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ActionManagementEffect (AddAgentAcquisitionVerb verb newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AgentAVManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (AgentAVManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ActionManagementEffect (AddAgentAcquisitionVerbPhrase phrase newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AgentAAManagementKey p _ -> p /= phrase; _ -> True) actionSet
        updatedActions = Data.Set.insert (AgentAAManagementKey phrase newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ActionManagementEffect (AddLocationAcquisitionVerb verb newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case LocationAVManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (LocationAVManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ActionManagementEffect (AddLocationAcquisitionVerbPhrase phrase newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case LocationAAManagementKey p _ -> p /= phrase; _ -> True) actionSet
        updatedActions = Data.Set.insert (LocationAAManagementKey phrase newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ActionManagementEffect (AddObjectDirectionalStimulus verb newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ObjectDSAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (ObjectDSAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (ActionManagementEffect (AddLocationAcquisitionVerb verb newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case LocationAVManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (LocationAVManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (ActionManagementEffect (AddLocationAcquisitionVerbPhrase phrase newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case LocationAAManagementKey p _ -> p /= phrase; _ -> True) actionSet
        updatedActions = Data.Set.insert (LocationAAManagementKey phrase newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (ActionManagementEffect (AddAgentDirectionalStimulus verb newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AgentDSAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (AgentDSAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (ActionManagementEffect (AddLocationDirectionalStimulus verb newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case LocationDSAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (LocationDSAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (ActionManagementEffect (AddAgentAcquisitionVerb verb newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AgentAVManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (AgentAVManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (ActionManagementEffect (AddAgentAcquisitionVerbPhrase phrase newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AgentAAManagementKey p _ -> p /= phrase; _ -> True) actionSet
        updatedActions = Data.Set.insert (AgentAAManagementKey phrase newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (ActionManagementEffect (AddObjectAcquisitionVerb verb newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ObjectAVManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (ObjectAVManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (ActionManagementEffect (AddObjectAcquisitionVerbPhrase phrase newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ObjectAAManagementKey p _ -> p /= phrase; _ -> True) actionSet
        updatedActions = Data.Set.insert (ObjectAAManagementKey phrase newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (ActionManagementEffect (AddContainerAcquisitionVerb verb newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ContainerAVManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (ContainerAVManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (ActionManagementEffect (AddContainerAcquisitionVerbPhrase phrase newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ContainerAAManagementKey p _ -> p /= phrase; _ -> True) actionSet
        updatedActions = Data.Set.insert (ContainerAAManagementKey phrase newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (ActionManagementEffect (AddObjectDirectionalStimulus verb newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ObjectDSAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (ObjectDSAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ActionManagementEffect (AddAgentDirectionalContainerStimulus verb newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AgentDSAContainerManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (AgentDSAContainerManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ActionManagementEffect (AddContainerDirectionalContainerStimulus verb newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ContainerDSAContainerManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (ContainerDSAContainerManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ActionManagementEffect (AddLocationDirectionalContainerStimulus verb newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case LocationDSAContainerManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (LocationDSAContainerManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (ActionManagementEffect (AddAgentDirectionalContainerStimulus verb newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AgentDSAContainerManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (AgentDSAContainerManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (ActionManagementEffect (AddContainerDirectionalContainerStimulus verb newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ContainerDSAContainerManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (ContainerDSAContainerManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (ActionManagementEffect (AddLocationDirectionalContainerStimulus verb newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case LocationDSAContainerManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (LocationDSAContainerManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyLocation lid)) (ActionManagementEffect (AddAgentDirectionalContainerStimulus verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AgentDSAContainerManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (AgentDSAContainerManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (ActionManagementEffect (AddAgentDirectionalContainerStimulus verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AgentDSAContainerManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (AgentDSAContainerManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyLocation lid)) (ActionManagementEffect (AddContainerDirectionalContainerStimulus verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ContainerDSAContainerManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (ContainerDSAContainerManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (ActionManagementEffect (AddContainerDirectionalContainerStimulus verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ContainerDSAContainerManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (ContainerDSAContainerManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyLocation lid)) (ActionManagementEffect (AddLocationDirectionalContainerStimulus verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case LocationDSAContainerManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (LocationDSAContainerManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (ActionManagementEffect (AddLocationDirectionalContainerStimulus verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case LocationDSAContainerManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (LocationDSAContainerManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (ActionManagementEffect (AddConsumption verb _targetOid newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case CAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (CAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyLocation lid)) (ActionManagementEffect (AddPositivePostural verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case PPManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (PPManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (ActionManagementEffect (AddPositivePostural verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case PPManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (PPManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyLocation lid)) (ActionManagementEffect (AddNegativePostural verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case NPManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (NPManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyLocation lid)) (ActionManagementEffect (AddConsumption verb _targetOid newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case CAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (CAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (ActionManagementEffect (AddLocationDirectionalStimulus verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case LocationDSAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (LocationDSAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyLocation lid)) (ActionManagementEffect (AddLocationDirectionalStimulus verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case LocationDSAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (LocationDSAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (ActionManagementEffect (AddObjectDirectionalStimulus verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ObjectDSAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (ObjectDSAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyLocation lid)) (ActionManagementEffect (AddObjectDirectionalStimulus verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ObjectDSAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (ObjectDSAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyLocation lid)) (ActionManagementEffect (AddObjectAcquisitionVerb verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ObjectAVManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (ObjectAVManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (ActionManagementEffect (AddObjectAcquisitionVerb verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ObjectAVManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (ObjectAVManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyLocation lid)) (ActionManagementEffect (AddObjectAcquisitionVerbPhrase phrase newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ObjectAAManagementKey p _ -> p /= phrase; _ -> True) actionSet
        updatedActions = Data.Set.insert (ObjectAAManagementKey phrase newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (ActionManagementEffect (AddObjectAcquisitionVerbPhrase phrase newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ObjectAAManagementKey p _ -> p /= phrase; _ -> True) actionSet
        updatedActions = Data.Set.insert (ObjectAAManagementKey phrase newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyLocation lid)) (ActionManagementEffect (AddLocationAcquisitionVerb verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case LocationAVManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (LocationAVManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (ActionManagementEffect (AddLocationAcquisitionVerb verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case LocationAVManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (LocationAVManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyLocation lid)) (ActionManagementEffect (AddLocationAcquisitionVerbPhrase phrase newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case LocationAAManagementKey p _ -> p /= phrase; _ -> True) actionSet
        updatedActions = Data.Set.insert (LocationAAManagementKey phrase newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (ActionManagementEffect (AddLocationAcquisitionVerbPhrase phrase newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case LocationAAManagementKey p _ -> p /= phrase; _ -> True) actionSet
        updatedActions = Data.Set.insert (LocationAAManagementKey phrase newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyLocation lid)) (ActionManagementEffect (AddContainerAcquisitionVerb verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ContainerAVManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (ContainerAVManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (ActionManagementEffect (AddContainerAcquisitionVerb verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ContainerAVManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (ContainerAVManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyLocation lid)) (ActionManagementEffect (AddContainerAcquisitionVerbPhrase phrase newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ContainerAAManagementKey p _ -> p /= phrase; _ -> True) actionSet
        updatedActions = Data.Set.insert (ContainerAAManagementKey phrase newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (ActionManagementEffect (AddContainerAcquisitionVerbPhrase phrase newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ContainerAAManagementKey p _ -> p /= phrase; _ -> True) actionSet
        updatedActions = Data.Set.insert (ContainerAAManagementKey phrase newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (ActionManagementEffect (AddNegativePostural verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case NPManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (NPManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey _) (FieldUpdateEffect (ObjectShortName targetOid newShortName)) = do
  modifyObjectM targetOid $ \obj -> obj { _shortName = newShortName }

processEffect (PlayerKey _) (FieldUpdateEffect (ObjectDescription targetOid newDescription)) = do
  modifyObjectM targetOid $ \obj -> obj { _description = newDescription }

processEffect (PlayerKey _) (FieldUpdateEffect (LocationTitle targetLid newTitle)) = do
  modifyLocationM targetLid $ \loc -> loc { _title = newTitle }

processEffect (PlayerKey _) (FieldUpdateEffect (PlayerLocation newLocationGID)) = do
  modifyPlayerM $ \player -> player { _location = newLocationGID }

processEffect _ (NarrationEffect narrationComputation) = do
  processNarrationEffect narrationComputation

processNarrationEffect :: NarrationComputation -> GameComputation Identity ()
processNarrationEffect (StaticNarration text) =
  modifyNarration $ updateActionConsequence text

processNarrationEffect InventoryNarration = do
  topLevelInventoryObjects <- getTopLevelInventoryObjectsM
  case topLevelInventoryObjects of
    [] -> modifyNarration $ updateActionConsequence "You've got nothing but a terrible headache and a slight pang of regret."
    objects -> do
      objectNames <- mapM (fmap _shortName . getObjectM) objects
      let itemsList = Data.Text.intercalate ", " objectNames
          fullMessage = "You look through your inventory. You have a feeling all these things are very important somehow. You are carrying: " <> itemsList <> "."
      modifyNarration $ updateActionConsequence fullMessage

processNarrationEffect LookNarration = do
  youSeeM

processNarrationEffect (LookAtNarration objGID) = do
  obj <- getObjectM objGID
  world <- gets _world
  let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world

  -- Generate location-based narration
  case Data.Map.Strict.lookup objGID spatialMap of
    Just relationships -> do
      do
        -- Handle primary location relationship
        if Inventory `Data.Set.member` relationships then
         modifyNarration
           $ updateActionConsequence
               ("You're holding the "
                  <> _shortName obj
                  <> ". "
                  <> _description obj)
        else do
        -- Process each relationship in the set (only if not in inventory)
          Data.Foldable.for_ (Data.Set.toList relationships) $ \case
            SupportedBy supportGID -> do
              support <- getObjectM supportGID
              modifyNarration $ updateActionConsequence $
                "The " <> _shortName obj
                       <> " is on the "
                       <> _shortName support
                       <> ". " <> _description obj
            ContainedIn containerGID -> do
              container <- getObjectM containerGID
              modifyNarration $ updateActionConsequence $
                "The " <> _shortName obj <> " is inside the " <> _shortName container
            Supports oidSet ->
              unless (Data.Set.null oidSet) $ do
                supportedNames <- mapM (fmap _shortName . getObjectM) (Data.Set.toList oidSet)
                modifyNarration $ updateActionConsequence $
                  "On it you see: " <> intercalate ", " supportedNames
            Contains oidSet ->
              unless (Data.Set.null oidSet) $ do
                containedNames <- mapM (fmap _shortName . getObjectM) (Data.Set.toList oidSet)
                modifyNarration $ updateActionConsequence $
                  "Inside it you see: " <> intercalate ", " containedNames
            Inventory -> pure () -- won't reach here due to if/else
    Nothing ->
     error ("Object not found in spatial relationships." ++ show objGID)

processNarrationEffect (LookInNarration objGID) = do
  world <- gets _world
  let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world

  case Data.Map.Strict.lookup objGID spatialMap of
    Nothing -> pure ()  -- Object not found
    Just relationships -> do
      let supportedObjects = [oid | Supports oidSet <- Data.Set.toList relationships,
                                    oid <- Data.Set.toList oidSet]
      let containedObjects = [oid | Contains oidSet <- Data.Set.toList relationships,
                                    oid <- Data.Set.toList oidSet]
      -- Generate narration for supported objects
      unless (null supportedObjects) $ do
        supportedNames <- mapM (fmap _shortName . getObjectM) supportedObjects
        let onText = "On it you see: " <> Data.Text.intercalate ", " supportedNames
        modifyNarration $ updateActionConsequence onText

      -- Generate narration for contained objects
      unless (null containedObjects) $ do
        containedNames <- mapM (fmap _shortName . getObjectM) containedObjects
        let inText = "In it you see: " <> Data.Text.intercalate ", " containedNames
        modifyNarration $ updateActionConsequence inText
      when (null supportedObjects && null containedObjects) $
        modifyNarration $ updateActionConsequence "It's empty."

lookupContainerAccessVerbPhrase :: ContainerAccessVerbPhrase
                                     -> ActionManagementFunctions
                                     -> Maybe (GID ContainerAccessActionF)
lookupContainerAccessVerbPhrase cavp (ActionManagementFunctions actions) =
  listToMaybe [gid | CONManagementKey p gid <- Data.Set.toList actions, p == cavp]


lookupAgentDirectionalStimulus :: DirectionalStimulusVerb
                                    -> ActionManagementFunctions
                                    -> Maybe (GID AgentDirectionalStimulusActionF)
lookupAgentDirectionalStimulus verb (ActionManagementFunctions actions) =
  listToMaybe [gid | AgentDSAManagementKey v gid <- Data.Set.toList actions, v == verb]

lookupLocationDirectionalStimulus :: DirectionalStimulusVerb
                                       -> ActionManagementFunctions
                                       -> Maybe (GID LocationDirectionalStimulusActionF)
lookupLocationDirectionalStimulus verb (ActionManagementFunctions actions) =
  listToMaybe [gid | LocationDSAManagementKey v gid <- Data.Set.toList actions, v == verb]

lookupObjectDirectionalStimulus :: DirectionalStimulusVerb
                                     -> ActionManagementFunctions
                                     -> Maybe (GID ObjectDirectionalStimulusActionF)
lookupObjectDirectionalStimulus verb (ActionManagementFunctions actions) =
  listToMaybe [gid | ObjectDSAManagementKey v gid <- Data.Set.toList actions, v == verb]

lookupAgentDirectionalContainerStimulus :: DirectionalStimulusVerb
                                            -> ActionManagementFunctions
                                            -> Maybe (GID AgentDirectionalStimulusContainerActionF)
lookupAgentDirectionalContainerStimulus verb (ActionManagementFunctions actions) =
 listToMaybe [gid | AgentDSAContainerManagementKey v gid <- Data.Set.toList actions, v == verb]

lookupContainerDirectionalContainerStimulus :: DirectionalStimulusVerb
                                            -> ActionManagementFunctions
                                            -> Maybe (GID ContainerDirectionalStimulusContainerActionF)
lookupContainerDirectionalContainerStimulus verb (ActionManagementFunctions actions) =
  listToMaybe [gid | ContainerDSAContainerManagementKey v gid <- Data.Set.toList actions, v == verb]

lookupLocationDirectionalContainerStimulus :: DirectionalStimulusVerb
                                             -> ActionManagementFunctions
                                             -> Maybe (GID LocationDirectionalStimulusContainerActionF)
lookupLocationDirectionalContainerStimulus verb (ActionManagementFunctions actions) =
  listToMaybe [gid | LocationDSAContainerManagementKey v gid <- Data.Set.toList actions, v == verb]

lookupAgentImplicitStimulus :: ImplicitStimulusVerb
                                 -> ActionManagementFunctions
                                 -> Maybe (GID AgentImplicitStimulusActionF)
lookupAgentImplicitStimulus verb (ActionManagementFunctions actions) =
  listToMaybe [gid | AgentISAManagementKey v gid <- Data.Set.toList actions, v == verb]

lookupLocationImplicitStimulus :: ImplicitStimulusVerb
                                    -> ActionManagementFunctions
                                    -> Maybe (GID LocationImplicitStimulusActionF)
lookupLocationImplicitStimulus verb (ActionManagementFunctions actions) =
  listToMaybe [gid | LocationISAManagementKey v gid <- Data.Set.toList actions, v == verb]

lookupSomaticAccess :: SomaticAccessVerb
                         -> ActionManagementFunctions
                         -> Maybe (GID SomaticAccessActionF)
lookupSomaticAccess verb (ActionManagementFunctions actions) =
  listToMaybe [gid | SSAManagementKey v gid <- Data.Set.toList actions, v == verb]

simplifyAcquisitionVerbPhrase :: AcquisitionVerbPhrase -> AcquisitionVerb
simplifyAcquisitionVerbPhrase (SimpleAcquisitionVerbPhrase verb _) = verb
simplifyAcquisitionVerbPhrase (AcquisitionVerbPhrase verb _ _ _)   = verb

lookupConsumption :: ConsumptionVerb -> ActionManagementFunctions -> Maybe (GID ConsumptionActionF)
lookupConsumption verb (ActionManagementFunctions actions) =
  listToMaybe [gid | CAManagementKey v gid <- Data.Set.toList actions, v == verb]

lookupPostural :: PosturalVerbPhrase -> ActionManagementFunctions -> Maybe (GID PosturalActionF)
lookupPostural phrase (ActionManagementFunctions actions) = case phrase of
  PositivePosturalVerbPhrase verb _ ->
    listToMaybe [gid | PPManagementKey v gid <- Data.Set.toList actions, v == verb]
  NegativePosturalVerbPhrase verb _ ->
    listToMaybe [gid | NPManagementKey v gid <- Data.Set.toList actions, v == verb]

emptyActionManagement :: ActionManagementFunctions
emptyActionManagement = ActionManagementFunctions Data.Set.empty


findAgentAVKey :: AcquisitionVerb -> ActionManagementFunctions -> Maybe (GID AgentAcquisitionActionF)
findAgentAVKey verb (ActionManagementFunctions actionSet) =
  listToMaybe [gid | AgentAVManagementKey v gid <- Data.Set.toList actionSet, v == verb]

findAgentAAKey :: AcquisitionVerbPhrase -> ActionManagementFunctions -> Maybe (GID AgentAcquisitionActionF)
findAgentAAKey phrase (ActionManagementFunctions actionSet) =
  listToMaybe [gid | AgentAAManagementKey p gid <- Data.Set.toList actionSet, p == phrase]

findObjectAVKey :: AcquisitionVerb -> ActionManagementFunctions -> Maybe (GID ObjectAcquisitionActionF)
findObjectAVKey verb (ActionManagementFunctions actionSet) =
  listToMaybe [gid | ObjectAVManagementKey v gid <- Data.Set.toList actionSet, v == verb]

findContainerAVKey :: AcquisitionVerb -> ActionManagementFunctions -> Maybe (GID ContainerAcquisitionActionF)
findContainerAVKey verb (ActionManagementFunctions actionSet) =
  listToMaybe [gid | ContainerAVManagementKey v gid <- Data.Set.toList actionSet, v == verb]

findLocationAVKey :: AcquisitionVerb -> ActionManagementFunctions -> Maybe (GID LocationAcquisitionActionF)
findLocationAVKey verb (ActionManagementFunctions actionSet) =
  listToMaybe [gid | LocationAVManagementKey v gid <- Data.Set.toList actionSet, v == verb]

findSSAKey :: SomaticAccessVerb -> ActionManagementFunctions-> Maybe (GID SomaticAccessActionF)
findSSAKey verb (ActionManagementFunctions actionSet) =
  listToMaybe [gid | SSAManagementKey v gid <- Data.Set.toList actionSet, v == verb]

findCAKey :: ConsumptionVerb -> ActionManagementFunctions -> Maybe (GID ConsumptionActionF)
findCAKey verb (ActionManagementFunctions actionSet) =
  listToMaybe [gid | CAManagementKey v gid <- Data.Set.toList actionSet, v == verb]

findPPKey :: PositivePosturalVerb -> ActionManagementFunctions -> Maybe (GID PosturalActionF)
findPPKey verb (ActionManagementFunctions actionSet) =
  listToMaybe [gid | PPManagementKey v gid <- Data.Set.toList actionSet, v == verb]

findNPKey :: NegativePosturalVerb -> ActionManagementFunctions -> Maybe (GID PosturalActionF)
findNPKey verb (ActionManagementFunctions actionSet) =
  listToMaybe [gid | NPManagementKey v gid <- Data.Set.toList actionSet, v == verb]

findCONManagementKey :: ContainerAccessVerbPhrase
                          -> ActionManagementFunctions
                          -> Maybe (GID ContainerAccessActionF)
findCONManagementKey cavp (ActionManagementFunctions actionSet) =
  listToMaybe [gid | CONManagementKey p gid <- Data.Set.toList actionSet, p == cavp]

findSAForContainersKey :: SimpleAccessVerb -> ActionManagementFunctions -> Maybe (GID ContainerAccessActionF)
findSAForContainersKey verb (ActionManagementFunctions actionSet) =
  listToMaybe [gid | SAConManagementKey v gid <- Data.Set.toList actionSet, v == verb]
