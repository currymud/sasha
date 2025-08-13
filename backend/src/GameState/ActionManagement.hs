module GameState.ActionManagement where
import           Control.Monad.Identity        (Identity)
import           Control.Monad.State           (modify')
import qualified Data.Map.Strict
import           Data.Maybe                    (listToMaybe)
import           Data.Set                      (Set)
import qualified Data.Set
import           GameState                     (modifyLocationM, modifyObjectM)
import           GameState.Perception          (buildPerceptionMapFromObjects,
                                                computePerceivableObjects,
                                                modifyPerceptionMapM)
import           Model.GameState               (AcquisitionActionF,
                                                ActionEffectKey (LocationKey, ObjectKey, PlayerKey),
                                                ActionEffectMap (ActionEffectMap),
                                                ActionManagement (AAManagementKey, CAManagementKey, DSAManagementKey, ISAManagementKey, NPManagementKey, PPManagementKey, SSAManagementKey),
                                                ActionManagementFunctions (ActionManagementFunctions),
                                                ConsumptionActionF,
                                                DirectionalStimulusActionF,
                                                Effect (AcquisitionEffect, ConsumptionEffect, DirectionalStimulusEffect, ImplicitStimulusEffect, NegativePosturalEffect, PerceptionEffect, PositivePosturalEffect, SomaticAccessEffect),
                                                GameComputation,
                                                GameState (_player),
                                                ImplicitStimulusActionF,
                                                Location (_locationActionManagement),
                                                Object (_objectActionManagement),
                                                Player (_playerActions),
                                                PlayerKey (PlayerKeyObject),
                                                PosturalActionF,
                                                SomaticAccessActionF)
import           Model.GID                     (GID)
import           Model.Parser.Atomics.Verbs    (DirectionalStimulusVerb,
                                                ImplicitStimulusVerb,
                                                SomaticAccessVerb)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase,
                                                ConsumptionVerbPhrase (ConsumptionVerbPhrase),
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

modifyObjectActionManagementM :: GID Object
                              -> (ActionManagementFunctions -> ActionManagementFunctions)
                              -> GameComputation Identity ()
modifyObjectActionManagementM oid actionF =
  modifyObjectM oid $ \obj ->
    obj { _objectActionManagement = actionF (_objectActionManagement obj) }

-- | Process all effects in an ActionEffectMap
processAllEffects :: ActionEffectMap -> GameComputation Identity ()
processAllEffects (ActionEffectMap effectMap) = do
  mapM_ processEffectEntry (Data.Map.Strict.toList effectMap)
  where
    processEffectEntry :: (ActionEffectKey, Set Effect) -> GameComputation Identity ()
    processEffectEntry (effectKey, effects) =
      mapM_ (processEffect effectKey) (Data.Set.toList effects)

-- | Process a single effect for a specific target
processEffect :: ActionEffectKey -> Effect -> GameComputation Identity ()

-- LOCATION EFFECTS (updating location action management)
processEffect (LocationKey lid) (ImplicitStimulusEffect verb newActionGID) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ISAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (ISAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (DirectionalStimulusEffect verb newActionGID) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case DSAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (DSAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (SomaticAccessEffect verb newActionGID) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case SSAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (SSAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (AcquisitionEffect avp newActionGID) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AAManagementKey p _ -> p /= avp; _ -> True) actionSet
        updatedActions = Data.Set.insert (AAManagementKey avp newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (PositivePosturalEffect verb newActionGID) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case PPManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (PPManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (LocationKey lid) (NegativePosturalEffect verb newActionGID) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case NPManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (NPManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

-- OBJECT EFFECTS (updating object action management)
processEffect (ObjectKey oid) (DirectionalStimulusEffect verb newActionGID) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case DSAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (DSAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ImplicitStimulusEffect verb newActionGID) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ISAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (ISAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (SomaticAccessEffect verb newActionGID) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case SSAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (SSAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (AcquisitionEffect avp newActionGID) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AAManagementKey p _ -> p /= avp; _ -> True) actionSet
        updatedActions = Data.Set.insert (AAManagementKey avp newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ConsumptionEffect verb _targetOid newActionGID) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        -- Find existing consumption verb phrase for this verb
        existingCVP = case [cvp | CAManagementKey cvp@(ConsumptionVerbPhrase v _) _ <- Data.Set.toList actionSet, v == verb] of
          (foundCvp:_) -> foundCvp
          []           -> error "No existing consumption verb phrase found for effect"
        -- Remove old consumption actions for this verb
        filteredActions = Data.Set.filter filterAction actionSet
        -- Add new action with existing phrase
        updatedActions = Data.Set.insert (CAManagementKey existingCVP newActionGID) filteredActions
    in ActionManagementFunctions updatedActions
  where
    filterAction (CAManagementKey (ConsumptionVerbPhrase v _) _) = v /= verb
    filterAction _                                               = True

processEffect (ObjectKey oid) (PositivePosturalEffect verb newActionGID) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case PPManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (PPManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (NegativePosturalEffect verb newActionGID) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case NPManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (NPManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

-- PLAYER EFFECTS (updating player action management)
processEffect (PlayerKey (PlayerKeyObject oid)) (AcquisitionEffect avp newActionGID) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AAManagementKey p _ -> p /= avp; _ -> True) actionSet
        updatedActions = Data.Set.insert (AAManagementKey avp newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (PositivePosturalEffect verb newActionGID) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case PPManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (PPManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (NegativePosturalEffect verb newActionGID) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case NPManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (NPManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (ConsumptionEffect verb _targetOid newActionGID) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        -- Find existing consumption verb phrase for this verb
        existingCVP = case [cvp | CAManagementKey cvp@(ConsumptionVerbPhrase v _) _ <- Data.Set.toList actionSet, v == verb] of
          (foundCvp:_) -> foundCvp
          []           -> error "No existing consumption verb phrase found for player effect"
        -- Remove old consumption actions for this verb
        filteredActions = Data.Set.filter filterAction actionSet
        -- Add new action with existing phrase
        updatedActions = Data.Set.insert (CAManagementKey existingCVP newActionGID) filteredActions
    in ActionManagementFunctions updatedActions
  where
    filterAction (CAManagementKey (ConsumptionVerbPhrase v _) _) = v /= verb
    filterAction _                                               = True

-- PERCEPTION EFFECTS (global perception updates)
processEffect _ PerceptionEffect = do
  perceivableObjects <- computePerceivableObjects
  newPerceptionMap <- buildPerceptionMapFromObjects (Data.Set.toList perceivableObjects)
  modifyPerceptionMapM (const newPerceptionMap)

-- Catch-all for unhandled effect/key combinations
processEffect effectKey effect = do
  error $ "Unhandled effect combination: " ++ show effectKey ++ " -> " ++ show effect

lookupDirectionalStimulus :: DirectionalStimulusVerb -> ActionManagementFunctions -> Maybe (GID DirectionalStimulusActionF)
lookupDirectionalStimulus verb (ActionManagementFunctions actions) =
  listToMaybe [gid | DSAManagementKey v gid <- Data.Set.toList actions, v == verb]

lookupImplicitStimulus :: ImplicitStimulusVerb -> ActionManagementFunctions -> Maybe (GID ImplicitStimulusActionF)
lookupImplicitStimulus verb (ActionManagementFunctions actions) =
  listToMaybe [gid | ISAManagementKey v gid <- Data.Set.toList actions, v == verb]

lookupSomaticAccess :: SomaticAccessVerb -> ActionManagementFunctions -> Maybe (GID SomaticAccessActionF)
lookupSomaticAccess verb (ActionManagementFunctions actions) =
  listToMaybe [gid | SSAManagementKey v gid <- Data.Set.toList actions, v == verb]

lookupAcquisition :: AcquisitionVerbPhrase -> ActionManagementFunctions -> Maybe (GID AcquisitionActionF)
lookupAcquisition phrase (ActionManagementFunctions actions) =
  listToMaybe [gid | AAManagementKey p gid <- Data.Set.toList actions, p == phrase]

lookupConsumption :: ConsumptionVerbPhrase -> ActionManagementFunctions -> Maybe (GID ConsumptionActionF)
lookupConsumption phrase (ActionManagementFunctions actions) =
  listToMaybe [gid | CAManagementKey p gid <- Data.Set.toList actions, p == phrase]

lookupPostural :: PosturalVerbPhrase -> ActionManagementFunctions -> Maybe (GID PosturalActionF)
lookupPostural phrase (ActionManagementFunctions actions) = case phrase of
  PositivePosturalVerbPhrase verb _ ->
    listToMaybe [gid | PPManagementKey v gid <- Data.Set.toList actions, v == verb]
  NegativePosturalVerbPhrase verb _ ->
    listToMaybe [gid | NPManagementKey v gid <- Data.Set.toList actions, v == verb]
-- Convenience builders
emptyActionManagement :: ActionManagementFunctions
emptyActionManagement = ActionManagementFunctions Data.Set.empty

