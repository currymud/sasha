{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Get (get,getDenied) where
import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader          (asks)
import           Control.Monad.State           (gets, modify')
import qualified Data.Map.Strict
import           Data.Set                      (Set, elemAt, filter, insert,
                                                null, toList)
import           Data.Text                     (Text)
import           GameState                     (getObjectM, modifyNarration,
                                                modifyObjectActionManagementM,
                                                parseAcquisitionPhrase)
import           GameState.ActionManagement    (lookupAcquisition)
import           GameState.Perception          (updatePerceptionMapM)
import           Model.GameState               (AcquisitionActionF (AcquiredFromF, AcquisitionActionF, RemovedFromF),
                                                ActionEffectKey (ObjectKey, PlayerKey),
                                                ActionEffectMap (ActionEffectMap, _actionEffectMap),
                                                ActionKeyMap (ActionKeyMap, _unActionKeyMap),
                                                ActionManagement (AAManagementKey, CAManagementKey, DSAManagementKey),
                                                ActionManagementFunctions (ActionManagementFunctions),
                                                ActionMaps (_acquisitionActionMap),
                                                Config (_actionMaps),
                                                Effect (AcquisitionEffect, ConsumptionEffect, DirectionalStimulusEffect),
                                                GameComputation,
                                                GameState (_player),
                                                Location (_locationActionManagement, _objectSemanticMap),
                                                Object (_objectActionManagement),
                                                Player (_actionKeyMap, _playerActions),
                                                PlayerKey (PlayerKeyObject),
                                                updateActionConsequence)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase (AcquisitionVerbPhrase),
                                                ConsumptionVerbPhrase (ConsumptionVerbPhrase))

getDenied :: AcquisitionActionF
getDenied = AcquisitionActionF (const (const (const denied)))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You try but feel dizzy and have to lay back down"

get :: AcquisitionActionF
get = AcquisitionActionF getit
  where
    getit :: Location -> ActionEffectMap -> AcquisitionVerbPhrase -> GameComputation Identity ()
    getit loc actionEffectMap avp = do
      executeLocationGet loc avp >>= \case
        Left locationAction -> locationAction  -- Location handles it, don't check object
        Right locationSuccess -> do
          -- Location allows acquisition, now check object
          executeObjectGet loc actionEffectMap avp >>= \case
            Left objectAction -> objectAction
            Right objectSuccess -> do
              locationSuccess
              objectSuccess
              -- PROCESS EFFECTS AFTER SUCCESSFUL ACQUISITION
              processAcquisitionEffects loc actionEffectMap avp

    processAcquisitionEffects :: Location -> ActionEffectMap -> AcquisitionVerbPhrase -> GameComputation Identity ()
    processAcquisitionEffects loc actionEffectMap avp = do
      let (objectPhrase, nounKey) = parseAcquisitionPhrase avp

      -- Find the object that was acquired
      case Data.Map.Strict.lookup nounKey loc._objectSemanticMap of
        Just objSet | not (Data.Set.null objSet) -> do
          let oid = Data.Set.elemAt 0 objSet
              ActionEffectMap effectMap = actionEffectMap
          mapM_ (processEffectEntry avp) (Data.Map.Strict.toList effectMap)
        _ -> pure ()

processEffectEntry :: AcquisitionVerbPhrase
                        -> (ActionEffectKey, Set Effect)
                        -> GameComputation Identity ()
processEffectEntry avp (effectKey, effects) = do
  mapM_ (processEffectWithKey avp effectKey) (Data.Set.toList effects)

processEffectWithKey :: AcquisitionVerbPhrase -> ActionEffectKey -> Effect -> GameComputation Identity ()
processEffectWithKey avp (PlayerKey (PlayerKeyObject oid)) (AcquisitionEffect _ newActionGID) = do
  -- Update the player's acquisition actions to use the new action for this phrase
  modify' $ \gs ->
    let player = gs._player
        ActionManagementFunctions playerActionSet = _playerActions player
        -- Remove any existing acquisition action for this phrase
        filteredActions = Data.Set.filter (\case AAManagementKey p _ -> p /= avp; _ -> True) playerActionSet
        -- Add the new action
        updatedActions = Data.Set.insert (AAManagementKey avp newActionGID) filteredActions
        updatedPlayerActions = ActionManagementFunctions updatedActions
        updatedPlayer = player { _playerActions = updatedPlayerActions }
    in gs { _player = updatedPlayer }

processEffectWithKey avp (ObjectKey oid) (DirectionalStimulusEffect verb newActionGID) = do
  -- Update the object's directional stimulus action
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case DSAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (DSAManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions
  updatePerceptionMapM oid

processEffectWithKey avp (ObjectKey oid) (ConsumptionEffect verb targetOid newActionGID) = do
  modifyObjectActionManagementM targetOid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        -- Remove old consumption actions for this verb
        filteredActions = Data.Set.filter filterAction actionSet
        -- Find existing consumption verb phrase to preserve
        existingCVP = case [cvp | CAManagementKey cvp@(ConsumptionVerbPhrase v _) _ <- Data.Set.toList actionSet, v == verb] of
          (foundCvp:_) -> foundCvp
          []           -> error "No existing consumption verb phrase found for effect"
        -- Add new action with existing phrase
        updatedActions = Data.Set.insert (CAManagementKey existingCVP newActionGID) filteredActions
    in ActionManagementFunctions updatedActions
  where
    filterAction (CAManagementKey (ConsumptionVerbPhrase v _) _) = v /= verb
    filterAction _                                               = True

processEffectWithKey _ _ _ = pure () -- Handle other effect types as needed
executeLocationGet :: Location -> AcquisitionVerbPhrase -> GameComputation Identity (Either (GameComputation Identity ()) (GameComputation Identity ()))
executeLocationGet loc avp = do
  let locationActionMgmt = _locationActionManagement loc
  case lookupAcquisition avp locationActionMgmt of
    Just actionGID -> do
      actionMap <- asks (_acquisitionActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Just (AcquisitionActionF locationAction) -> do
          pure $ Left $ locationAction loc (ActionEffectMap mempty) avp
        Just (RemovedFromF locationRemovedAction) -> do
          locationRemovedAction loc avp
        Just (AcquiredFromF locationAcquiredAction) -> do
          locationAcquiredAction loc avp
        _ -> pure $ Left $ modifyNarration $ updateActionConsequence "No location-specific action"
    Nothing -> pure $ Right $ modifyNarration $ updateActionConsequence "Location allows general acquisition"

executeObjectGet :: Location -> ActionEffectMap -> AcquisitionVerbPhrase -> GameComputation Identity (Either (GameComputation Identity ()) (GameComputation Identity ()))
executeObjectGet loc actionEffectMap avp = do
  let (objectPhrase, nounKey) = parseAcquisitionPhrase avp
  case Data.Map.Strict.lookup nounKey loc._objectSemanticMap of
    Just objSet | not (Data.Set.null objSet) -> do
      let oid = Data.Set.elemAt 0 objSet
      obj <- getObjectM oid
      let objectActionMgmt = _objectActionManagement obj
      case lookupAcquisition avp objectActionMgmt of
        Just objectActionGID -> do
          actionMap <- asks (_acquisitionActionMap . _actionMaps)
          case Data.Map.Strict.lookup objectActionGID actionMap of
            Just (AcquisitionActionF objectAction) -> do
              pure $ Left $ objectAction loc actionEffectMap avp
            Just (AcquiredFromF objectAcquiredAction) -> do
              objectAcquiredAction loc avp
            Just (RemovedFromF objectRemovedAction) -> do
              objectRemovedAction loc avp
            Nothing -> pure $ Left $ modifyNarration $ updateActionConsequence "Object action not found in action map"
        Nothing ->
          pure $ Right $ modifyNarration $ updateActionConsequence "Object allows general acquisition"
    _ -> pure $ Left $ modifyNarration $ updateActionConsequence "You don't see that here."
