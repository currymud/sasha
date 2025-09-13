module GameState.ActionManagement where
import           Control.Applicative           ((<|>))
import           Control.Monad.Identity        (Identity)
import           Control.Monad.State           (modify')
import qualified Data.Foldable
import qualified Data.Map.Strict
import           Data.Maybe                    (listToMaybe)
import           Data.Set                      (Set)
import qualified Data.Set
import           GameState                     (modifyLocationM, modifyObjectM,
                                                modifyPlayerM)
import           GameState.EffectRegistry      (lookupActionEffectsInRegistry)
import           Model.Core                    (AcquisitionActionF,
                                                TargetEffectKey (LocationKey, ObjectKey, PlayerKey),
                                                ActionEffectMap (ActionEffectMap),
                                                ActionManagement (AAManagementKey, AVManagementKey, CAManagementKey, CONManagementKey, DSAContainerManagementKey, DSAManagementKey, ISAManagementKey, NPManagementKey, PPManagementKey, SAConManagementKey, SSAManagementKey),
                                                ActionManagementFunctions (ActionManagementFunctions),
                                                ActionManagementOperation (AddAcquisitionVerb, AddAcquisitionVerbPhrase, AddConsumption, AddContainerAccess, AddContainerAccessVerb, AddDirectionalContainerStimulus, AddDirectionalStimulus, AddImplicitStimulus, AddNegativePostural, AddPositivePostural, AddSomaticAccess),
                                                ConsumptionActionF,
                                                ContainerAccessActionF,
                                                DirectionalStimulusActionF,
                                                DirectionalStimulusContainerActionF,
                                                Effect (ActionManagementEffect, FieldUpdateEffect),
                                                ActionEffectKey,
                                                FieldUpdateOperation (LocationTitle, ObjectDescription, ObjectShortName, PlayerLocation),
                                                GameComputation,
                                                GameState (_effectRegistry, _player, _systemEffectRegistry),
                                                ImplicitStimulusActionF,
                                                Location (_locationActionManagement),
                                                Object (_description, _objectActionManagement, _shortName),
                                                Player (_location, _playerActions),
                                                PlayerKey (PlayerKeyLocation, PlayerKeyObject),
                                                PosturalActionF,
                                                SomaticAccessActionF,
                                                SystemEffect,
                                                SystemEffectConfig,
                                                SystemEffectKey, _systemEffect,
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

processEffect (LocationKey lid) (ActionManagementEffect (AddImplicitStimulus verb newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ISAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (ISAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (ActionManagementEffect (AddDirectionalContainerStimulus verb newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case DSAContainerManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (DSAContainerManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (ActionManagementEffect (AddDirectionalStimulus verb newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case DSAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (DSAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (ActionManagementEffect (AddSomaticAccess verb newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case SSAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (SSAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (ActionManagementEffect (AddAcquisitionVerb verb newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AVManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (AVManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (ActionManagementEffect (AddAcquisitionVerbPhrase phrase newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AAManagementKey p _ -> p /= phrase; _ -> True) actionSet
        updatedActions = Data.Set.insert (AAManagementKey phrase newActionGID) filteredActions
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

-- OBJECT EFFECTS (updating object action management)
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

processEffect (ObjectKey oid) (ActionManagementEffect (AddImplicitStimulus verb newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ISAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (ISAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ActionManagementEffect (AddDirectionalStimulus verb newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case DSAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (DSAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ActionManagementEffect (AddDirectionalContainerStimulus verb newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case DSAContainerManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (DSAContainerManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ActionManagementEffect (AddSomaticAccess verb newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case SSAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (SSAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ActionManagementEffect (AddAcquisitionVerb verb newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AVManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (AVManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ActionManagementEffect (AddAcquisitionVerbPhrase phrase newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AAManagementKey p _ -> p /= phrase; _ -> True) actionSet
        updatedActions = Data.Set.insert (AAManagementKey phrase newActionGID) filteredActions
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

-- PLAYER EFFECTS (updating player action management)
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
processEffect (PlayerKey (PlayerKeyLocation lid)) (ActionManagementEffect (AddImplicitStimulus verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ISAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (ISAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (ActionManagementEffect (AddImplicitStimulus verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ISAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (ISAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (ActionManagementEffect (AddDirectionalStimulus verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case DSAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (DSAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyLocation lid)) (ActionManagementEffect (AddDirectionalStimulus verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case DSAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (DSAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (ActionManagementEffect (AddDirectionalContainerStimulus verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case DSAContainerManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (DSAContainerManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyLocation lid)) (ActionManagementEffect (AddDirectionalContainerStimulus verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case DSAContainerManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (DSAContainerManagementKey verb newActionGID) filteredActions
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

processEffect (PlayerKey pk) (ActionManagementEffect (AddAcquisitionVerb verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AVManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (AVManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey _) (ActionManagementEffect (AddAcquisitionVerbPhrase phrase newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AAManagementKey p _ -> p /= phrase; _ -> True) actionSet
        updatedActions = Data.Set.insert (AAManagementKey phrase newActionGID) filteredActions
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

lookupContainerAccessVerbPhrase :: ContainerAccessVerbPhrase -> ActionManagementFunctions -> Maybe (GID ContainerAccessActionF)
lookupContainerAccessVerbPhrase cavp (ActionManagementFunctions actions) =
  listToMaybe [gid | CONManagementKey p gid <- Data.Set.toList actions, p == cavp]

lookupDirectionalStimulus :: DirectionalStimulusVerb -> ActionManagementFunctions -> Maybe (GID DirectionalStimulusActionF)
lookupDirectionalStimulus verb (ActionManagementFunctions actions) =
  listToMaybe [gid | DSAManagementKey v gid <- Data.Set.toList actions, v == verb]

lookupImplicitStimulus :: ImplicitStimulusVerb -> ActionManagementFunctions -> Maybe (GID ImplicitStimulusActionF)
lookupImplicitStimulus verb (ActionManagementFunctions actions) =
  listToMaybe [gid | ISAManagementKey v gid <- Data.Set.toList actions, v == verb]

lookupSomaticAccess :: SomaticAccessVerb -> ActionManagementFunctions -> Maybe (GID SomaticAccessActionF)
lookupSomaticAccess verb (ActionManagementFunctions actions) =
  listToMaybe [gid | SSAManagementKey v gid <- Data.Set.toList actions, v == verb]

lookupAcquisitionPhrase :: AcquisitionVerbPhrase -> ActionManagementFunctions -> Maybe (GID AcquisitionActionF)
lookupAcquisitionPhrase avp (ActionManagementFunctions actions) =
  -- First try exact phrase match
  listToMaybe [gid | AAManagementKey p gid <- Data.Set.toList actions, p == avp]
    <|>
  -- Try simplified phrase match (new!)
  (case avp of
    AcquisitionVerbPhrase verb objPhrase _ _ ->
      listToMaybe [gid | AAManagementKey (SimpleAcquisitionVerbPhrase v op) gid <- Data.Set.toList actions,
                         v == verb && op == objPhrase]
    _ -> Nothing)
    <|>
  -- Then try just the verb
  listToMaybe [gid | AVManagementKey v gid <- Data.Set.toList actions, v == simplifyAcquisitionVerbPhrase avp]

simplifyAcquisitionVerbPhrase :: AcquisitionVerbPhrase -> AcquisitionVerb
simplifyAcquisitionVerbPhrase (SimpleAcquisitionVerbPhrase verb _) = verb
simplifyAcquisitionVerbPhrase (AcquisitionVerbPhrase verb _ _ _)   = verb

lookupDirectionalContainerStimulus :: DirectionalStimulusVerb
                                        -> ActionManagementFunctions
                                        -> Maybe (GID DirectionalStimulusContainerActionF)
lookupDirectionalContainerStimulus verb (ActionManagementFunctions actions) =
  listToMaybe [gid | DSAContainerManagementKey v gid <- Data.Set.toList actions, v == verb]

lookupConsumption :: ConsumptionVerb -> ActionManagementFunctions -> Maybe (GID ConsumptionActionF)
lookupConsumption verb (ActionManagementFunctions actions) =
  listToMaybe [gid | CAManagementKey v gid <- Data.Set.toList actions, v == verb]

lookupPostural :: PosturalVerbPhrase -> ActionManagementFunctions -> Maybe (GID PosturalActionF)
lookupPostural phrase (ActionManagementFunctions actions) = case phrase of
  PositivePosturalVerbPhrase verb _ ->
    listToMaybe [gid | PPManagementKey v gid <- Data.Set.toList actions, v == verb]
  NegativePosturalVerbPhrase verb _ ->
    listToMaybe [gid | NPManagementKey v gid <- Data.Set.toList actions, v == verb]
-- Convenience builders
emptyActionManagement :: ActionManagementFunctions
emptyActionManagement = ActionManagementFunctions Data.Set.empty

findDSAKey :: DirectionalStimulusVerb -> ActionManagementFunctions -> Maybe (GID DirectionalStimulusActionF)
findDSAKey verb (ActionManagementFunctions actionSet) =
  listToMaybe [gid | DSAManagementKey v gid <- Data.Set.toList actionSet, v == verb]

findAVKey :: AcquisitionVerb -> ActionManagementFunctions -> Maybe (GID AcquisitionActionF)
findAVKey verb (ActionManagementFunctions actionSet) =
  listToMaybe [gid | AVManagementKey v gid <- Data.Set.toList actionSet, v == verb]

findISAKey :: ImplicitStimulusVerb -> ActionManagementFunctions -> Maybe (GID ImplicitStimulusActionF)
findISAKey verb (ActionManagementFunctions actionSet) =
  listToMaybe [gid | ISAManagementKey v gid <- Data.Set.toList actionSet, v == verb]

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
