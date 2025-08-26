module GameState.ActionManagement where
import           Control.Monad.Identity        (Identity)
import           Control.Monad.State           (MonadState (get), gets, modify')
import qualified Data.Map.Strict
import           Data.Maybe                    (listToMaybe)
import           Data.Set                      (Set)
import qualified Data.Set
import           Debug.Trace                   (trace)
import           GameState                     (modifyLocationM, modifyObjectM)
import           GameState.EffectRegistry      (lookupActionEffectsInRegistry)
import           GameState.Perception          (buildPerceptionMapFromObjects,
                                                computePerceivableObjects,
                                                modifyPerceptionMapM, youSeeM)
import           Model.GameState               (AcquisitionActionF,
                                                ActionEffectKey (LocationKey, ObjectKey, PlayerKey),
                                                ActionEffectMap (ActionEffectMap),
                                                ActionGID (AcquisitionActionGID, PosturalActionGID, SomaticAccessActionGID),
                                                ActionManagement (AAManagementKey, AVManagementKey, CAManagementKey, DSAManagementKey, ISAManagementKey, NPManagementKey, PPManagementKey, SSAManagementKey),
                                                ActionManagementFunctions (ActionManagementFunctions),
                                                ActionManagementOperation (AddAcquisitionPhrase, AddAcquisitionVerb, AddConsumption, AddDirectionalStimulus, AddImplicitStimulus, AddNegativePostural, AddPositivePostural, AddSomaticAccess),
                                                ConsumptionActionF,
                                                DirectionalStimulusActionF,
                                                Effect (ActionManagementEffect, FieldUpdateEffect),
                                                EffectActionKey,
                                                FieldUpdateOperation (LocationTitle, ObjectDescription, ObjectShortName, PlayerLocation),
                                                GameComputation,
                                                GameState (_effectRegistry, _player, _systemEffectRegistry),
                                                ImplicitStimulusActionF,
                                                Location (_locationActionManagement),
                                                Object (_description, _objectActionManagement, _shortName),
                                                Player (_location, _playerActions),
                                                PlayerKey (PlayerKeyObject),
                                                PosturalActionF,
                                                SomaticAccessActionF,
                                                SystemEffect,
                                                SystemEffectConfig (SystemEffectConfig, _systemEffectManagement),
                                                SystemEffectKey, _systemEffect,
                                                _title)
import           Model.GID                     (GID)
import           Model.Parser.Atomics.Verbs    (AcquisitionVerb,
                                                DirectionalStimulusVerb,
                                                ImplicitStimulusVerb,
                                                NegativePosturalVerb,
                                                PositivePosturalVerb,
                                                SomaticAccessVerb)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase (AcquisitionVerbPhrase, SimpleAcquisitionVerbPhrase),
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

registerSystemEffect :: SystemEffectKey -> GID SystemEffect -> SystemEffectConfig -> GameComputation Identity ()
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


processEffectsFromRegistry :: EffectActionKey -> GameComputation Identity ()
processEffectsFromRegistry actionKey = do
  trace ("DEBUG: processEffectsFromRegistry called with: " ++ show actionKey) $ pure ()
  gameState <- get
  let registry = _effectRegistry gameState
  trace ("DEBUG: Full registry keys: " ++ show (Data.Map.Strict.keys registry)) $ pure ()
  maybeEffectMap <- lookupActionEffectsInRegistry actionKey
  case maybeEffectMap of
    Just effectMap -> do
      let ActionEffectMap effectMapContents = effectMap
      trace ("DEBUG: Found effect map with keys: " ++ show (Data.Map.Strict.keys effectMapContents)) $ pure ()
      processAllEffects effectMap
    Nothing -> do
      trace ("DEBUG: No effects found for key: " ++ show actionKey) $ pure ()

modifyObjectActionManagementM :: GID Object
                             -> (ActionManagementFunctions -> ActionManagementFunctions)
                             -> GameComputation Identity ()
modifyObjectActionManagementM oid actionF = do
 modifyObjectM oid $ \obj ->
   obj { _objectActionManagement = actionF (_objectActionManagement obj) }

processAllEffects :: ActionEffectMap -> GameComputation Identity ()
processAllEffects (ActionEffectMap effectMap) = do
  trace ("DEBUG: processAllEffects called with effect keys: " ++ show (Data.Map.Strict.keys effectMap)) $ pure ()
  mapM_ processEffectEntry (Data.Map.Strict.toList effectMap)
  where
    processEffectEntry :: (ActionEffectKey, Set Effect) -> GameComputation Identity ()
    processEffectEntry (effectKey, effects) = do
      trace ("DEBUG: Processing effect key: " ++ show effectKey ++ " with " ++ show (Data.Set.size effects) ++ " effects") $ pure ()
      mapM_ (processEffect effectKey) (Data.Set.toList effects)

-- processEffect implementation remains the same as in your original code
-- Updated processEffect patterns for GameState.ActionManagement
-- Replace all the existing processEffect implementations with these:

-- LOCATION EFFECTS (updating location action management)
processEffect :: ActionEffectKey -> Effect -> GameComputation Identity ()
processEffect (LocationKey lid) (ActionManagementEffect (AddImplicitStimulus verb newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ISAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (ISAManagementKey verb newActionGID) filteredActions
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

processEffect (LocationKey lid) (ActionManagementEffect (AddAcquisitionPhrase avp newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AAManagementKey p _ -> p /= avp; _ -> True) actionSet
        updatedActions = Data.Set.insert (AAManagementKey avp newActionGID) filteredActions
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

-- OBJECT EFFECTS (updating object action management)
processEffect (ObjectKey oid) (ActionManagementEffect (AddImplicitStimulus verb newActionGID) _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case ISAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (ISAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ActionManagementEffect (AddDirectionalStimulus verb newActionGID) _) = do
  trace ("processEffect: Processing DirectionalStimulusEffect for object " ++ show oid ++ " with verb " ++ show verb ++ " and new action " ++ show newActionGID) $
    modifyObjectActionManagementM oid $ \actionMgmt ->
      trace ("processEffect: OBJECT DSA LAMBDA EXECUTING - Inside modifyObjectActionManagementM lambda for object " ++ show oid) $
      let ActionManagementFunctions actionSet = actionMgmt
          oldLookActions = [gid | DSAManagementKey v gid <- Data.Set.toList actionSet, v == verb]
          filteredActions = Data.Set.filter (\case DSAManagementKey v _ -> v /= verb; _ -> True) actionSet
          updatedActions = Data.Set.insert (DSAManagementKey verb newActionGID) filteredActions
      in trace ("processEffect: OBJECT DSA - Old look actions for verb: " ++ show oldLookActions) $
         trace ("processEffect: OBJECT DSA LAMBDA COMPLETE - Original: " ++ show (Data.Set.toList actionSet) ++ " -> Updated: " ++ show (Data.Set.toList updatedActions)) $
         ActionManagementFunctions updatedActions

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

processEffect (ObjectKey oid) (ActionManagementEffect (AddAcquisitionPhrase avp newActionGID) _) = do
  trace ("processEffect: Processing AcquisitionPhraseEffect for object " ++ show oid ++ " with phrase " ++ show avp ++ " and new action " ++ show newActionGID) $
    modifyObjectActionManagementM oid $ \actionMgmt ->
      trace ("processEffect: LAMBDA EXECUTING - Inside modifyObjectActionManagementM lambda") $
      let ActionManagementFunctions actionSet = actionMgmt
          matchingActions = [gid | AAManagementKey p gid <- Data.Set.toList actionSet, p == avp]
          nonMatchingActions = [gid | AAManagementKey p gid <- Data.Set.toList actionSet, p /= avp]
          filteredActions = Data.Set.filter (\case AAManagementKey p _ -> p /= avp; _ -> True) actionSet
          updatedActions = Data.Set.insert (AAManagementKey avp newActionGID) filteredActions
      in trace ("processEffect: MATCHING actions with same phrase: " ++ show matchingActions) $
         trace ("processEffect: NON-MATCHING actions: " ++ show nonMatchingActions) $
         trace ("processEffect: LAMBDA COMPLETE - Original: " ++ show (Data.Set.toList actionSet) ++ " -> Updated: " ++ show (Data.Set.toList updatedActions)) $
         ActionManagementFunctions updatedActions

processEffect (ObjectKey oid) (ActionManagementEffect (AddConsumption verb _targetOid newActionGID) _) = do
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

-- PLAYER EFFECTS (updating player action management)
processEffect (PlayerKey (PlayerKeyObject oid)) (ActionManagementEffect (AddAcquisitionPhrase verb newActionGID) _) = do
  trace ("processEffect: Processing AcquisitionPhraseEffect for PLAYER related to object " ++ show oid ++ " with phrase " ++ show verb ++ " and new action " ++ show newActionGID) $
    modifyPlayerActionManagementM $ \actionMgmt ->
      trace ("processEffect: PLAYER LAMBDA EXECUTING - Inside modifyPlayerActionManagementM lambda") $
      let ActionManagementFunctions actionSet = actionMgmt
          matchingActions = [gid | AAManagementKey p gid <- Data.Set.toList actionSet, p == verb]
          nonMatchingActions = [gid | AAManagementKey p gid <- Data.Set.toList actionSet, p /= verb]
          filteredActions = Data.Set.filter (\case AAManagementKey p _ -> p /= verb; _ -> True) actionSet
          updatedActions = Data.Set.insert (AAManagementKey verb newActionGID) filteredActions
      in trace ("processEffect: PLAYER MATCHING actions with same phrase: " ++ show matchingActions) $
         trace ("processEffect: PLAYER NON-MATCHING actions: " ++ show nonMatchingActions) $
         trace ("processEffect: PLAYER LAMBDA COMPLETE - Original: " ++ show (Data.Set.toList actionSet) ++ " -> Updated: " ++ show (Data.Set.toList updatedActions)) $
         ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (ActionManagementEffect (AddAcquisitionVerb verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AVManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (AVManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (ActionManagementEffect (AddPositivePostural verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case PPManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (PPManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (ActionManagementEffect (AddNegativePostural verb newActionGID) _) = do
  modifyPlayerActionManagementM $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case NPManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (NPManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions

processEffect (PlayerKey (PlayerKeyObject oid)) (ActionManagementEffect (AddConsumption verb _targetOid newActionGID) _) = do
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

-- FIELD UPDATE EFFECTS (these would need implementations for updating game state fields)
-- Note: These patterns assume you have field update functions available
-- You may need to implement these functions based on your architecture

-- For ObjectKey field updates, we need to extract the object GID from the ActionGID
processEffect (ObjectKey targetOid) (FieldUpdateEffect (ObjectShortName newShortName) actionGID) = do
  case actionGID of
    AcquisitionActionGID _ -> modifyObjectM targetOid $ \obj -> obj { _shortName = newShortName }
    _ -> error $ "Unhandled ActionGID type for ObjectShortName update: " ++ show actionGID

processEffect (ObjectKey targetOid) (FieldUpdateEffect (ObjectDescription newDescription) actionGID) = do
  case actionGID of
    AcquisitionActionGID _ -> modifyObjectM targetOid $ \obj -> obj { _description = newDescription }
    _ -> error $ "Unhandled ActionGID type for ObjectDescription update: " ++ show actionGID

-- For LocationKey field updates
processEffect (LocationKey targetLid) (FieldUpdateEffect (LocationTitle newTitle) actionGID) = do
  case actionGID of
    SomaticAccessActionGID _ -> modifyLocationM targetLid $ \loc -> loc { _title = newTitle }
    _ -> error $ "Unhandled ActionGID type for LocationTitle update: " ++ show actionGID

-- For PlayerKey field updates
processEffect (PlayerKey _) (FieldUpdateEffect (PlayerLocation newLocationGID) actionGID) = do
  case actionGID of
    PosturalActionGID _ -> modifyPlayerM $ \player -> player { _location = newLocationGID }
    _ -> error $ "Unhandled ActionGID type for PlayerLocation update: " ++ show actionGID

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

lookupAcquisitionVerbPhrase :: AcquisitionVerbPhrase -> ActionManagementFunctions -> Maybe (GID AcquisitionActionF)
lookupAcquisitionVerbPhrase avp (ActionManagementFunctions actions) =
  let result = listToMaybe [gid | AAManagementKey p gid <- Data.Set.toList actions, p == avp]
      allAAActions = [show (p, gid) | AAManagementKey p gid <- Data.Set.toList actions]
  in trace ("lookupAcquisitionVerbPhrase: Looking for " ++ show avp ++ " in actions: " ++ show allAAActions ++ " -> result: " ++ show result) result

lookupAcquisition verb (ActionManagementFunctions actions) =
  let phraseMatches = [gid | AAManagementKey phrase gid <- Data.Set.toList actions,
                             let matches = matchesVerb phrase
                             in trace ("DEBUG: matchesVerb " ++ show phrase ++ " = " ++ show matches) matches]
      verbMatches = [gid | AVManagementKey v gid <- Data.Set.toList actions, v == verb]
      result = listToMaybe (phraseMatches ++ verbMatches)
  in trace ("DEBUG: phraseMatches: " ++ show phraseMatches ++ ", verbMatches: " ++ show verbMatches) $
     trace ("DEBUG: lookupAcquisition result: " ++ show result) result
  where
    matchesVerb :: AcquisitionVerbPhrase -> Bool
    matchesVerb (SimpleAcquisitionVerbPhrase v _) = v == verb
    matchesVerb (AcquisitionVerbPhrase v _ _ _)   = v == verb

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

findDSAKey :: DirectionalStimulusVerb -> ActionManagementFunctions -> Maybe (GID DirectionalStimulusActionF)
findDSAKey verb (ActionManagementFunctions actionSet) =
  listToMaybe [gid | DSAManagementKey v gid <- Data.Set.toList actionSet, v == verb]

findAAKey :: AcquisitionVerbPhrase ->  ActionManagementFunctions -> Maybe (GID AcquisitionActionF)
findAAKey phrase (ActionManagementFunctions actionSet) =
  listToMaybe [gid | AAManagementKey p gid <- Data.Set.toList actionSet, p == phrase]

findAVKey :: AcquisitionVerb -> ActionManagementFunctions -> Maybe (GID AcquisitionActionF)
findAVKey verb (ActionManagementFunctions actionSet) =
  listToMaybe [gid | AVManagementKey v gid <- Data.Set.toList actionSet, v == verb]

findISAKey :: ImplicitStimulusVerb -> ActionManagementFunctions -> Maybe (GID ImplicitStimulusActionF)
findISAKey verb (ActionManagementFunctions actionSet) =
  listToMaybe [gid | ISAManagementKey v gid <- Data.Set.toList actionSet, v == verb]

findSSAKey :: SomaticAccessVerb -> ActionManagementFunctions-> Maybe (GID SomaticAccessActionF)
findSSAKey verb (ActionManagementFunctions actionSet) =
  listToMaybe [gid | SSAManagementKey v gid <- Data.Set.toList actionSet, v == verb]

findCAKey :: ConsumptionVerbPhrase -> ActionManagementFunctions -> Maybe (GID ConsumptionActionF)
findCAKey phrase (ActionManagementFunctions actionSet) =
  listToMaybe [gid | CAManagementKey p gid <- Data.Set.toList actionSet, p == phrase]

findPPKey :: PositivePosturalVerb -> ActionManagementFunctions -> Maybe (GID PosturalActionF)
findPPKey verb (ActionManagementFunctions actionSet) =
  listToMaybe [gid | PPManagementKey v gid <- Data.Set.toList actionSet, v == verb]

findNPKey :: NegativePosturalVerb -> ActionManagementFunctions -> Maybe (GID PosturalActionF)
findNPKey verb (ActionManagementFunctions actionSet) =
  listToMaybe [gid | NPManagementKey v gid <- Data.Set.toList actionSet, v == verb]

