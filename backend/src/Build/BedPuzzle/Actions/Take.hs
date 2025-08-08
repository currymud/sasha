{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Take () where
import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader          (asks)
import           Control.Monad.State           (modify')
import qualified Data.Map.Strict
import           Data.Set                      (Set, elemAt, null, toList)
import           Data.Text                     (Text)
import           GameState                     (getObjectM, modifyNarration,
                                                modifyObjectActionManagementM,
                                                parseAcquisitionPhrase,
                                                updatePerceptionMapM)
import           Model.GameState               (AcquisitionActionF (AcquiredFromF, AcquisitionActionF, RemovedFromF),
                                                ActionEffectKey (ObjectKey, PlayerKey),
                                                ActionEffectMap (ActionEffectMap, _actionEffectMap),
                                                ActionManagement (_acquisitionActionManagement, _consumptionActionManagement, _directionalStimulusActionManagement, _implicitStimulusActionManagement, _somaticStimulusActionManagement),
                                                ActionMaps (_consumptionActionMap),
                                                Config (_actionMaps),
                                                ConsumptionActionF (ConsumptionActionF, _consumptionAction),
                                                Effect (AcquisitionEffect, DirectionalStimulusEffect),
                                                GameComputation,
                                                GameState (_player),
                                                Location (_locationActionManagement, _objectSemanticMap),
                                                Object (_objectActionManagement),
                                                Player (_actionKeyMap, _playerActions),
                                                PlayerActions (_acquisitionActions),
                                                PlayerKey (PlayerKeyObject),
                                                updateActionConsequence)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase (AcquisitionVerbPhrase),
                                                ConsumptionVerbPhrase (ConsumptionVerbPhrase),
                                                Imperative (ConsumptionVerbPhrase'))
import           Prelude                       hiding (take)
  {-
takeDenied :: ConsumptionActionF
takeDenied = ConsumptionActionF (const (const (const denied)))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You try but feel dizzy and have to lay back down"

take :: ConsumptionActionF
take = ConsumptionActionF getit
  where
    getit :: Location
               -> ActionEffectMap
               -> ConsumptionVerbPhrase
               -> GameComputation Identity ()
    getit loc actionEffectMap cvp = do
      executeLocationGet loc cvp >>= \case
        Left locationAction -> locationAction  -- Location handles it, don't check object
        Right locationSuccess -> do
          -- Location allows acquisition, now check object
          executeObjectGet loc actionEffectMap cvp >>= \case
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

-- Add this new function that includes the ActionEffectKey
processEffectWithKey :: AcquisitionVerbPhrase -> ActionEffectKey -> Effect -> GameComputation Identity ()
processEffectWithKey avp (PlayerKey (PlayerKeyObject oid)) (AcquisitionEffect _ newActionGID) = do
  -- Update the player's acquisition actions to use the new action for this phrase
  modify' $ \gs ->
    let player = gs._player
        playerActions = _playerActions player
        acquisitionActions = _acquisitionActions playerActions
        updatedAcquisitionActions = Data.Map.Strict.insert avp newActionGID acquisitionActions
        updatedPlayerActions = playerActions { _acquisitionActions = updatedAcquisitionActions }
        updatedPlayer = player { _playerActions = updatedPlayerActions }
    in gs { _player = updatedPlayer }

processEffectWithKey avp (ObjectKey oid) (DirectionalStimulusEffect verb newActionGID) = do
  -- Update the object's directional stimulus action
  modifyObjectActionManagementM oid $ \actionMgmt ->
    let directionalMap = _directionalStimulusActionManagement actionMgmt
        updatedMap = Data.Map.Strict.insert verb newActionGID directionalMap
    in actionMgmt { _directionalStimulusActionManagement = updatedMap }
  updatePerceptionMapM oid

processEffectWithKey _ _ _ = pure () -- Handle other effect types as needed

executeLocationGet :: Location
                        -> ConsumptionVerbPhrase
                        -> GameComputation Identity (Either (GameComputation Identity ()) (GameComputation Identity ()))
executeLocationGet loc avp = do
  -- Look up location's acquisition actions
  let locationConsumptionActions = _consumptionActionManagement (_locationActionManagement loc)

  case Data.Map.Strict.lookup avp locationAcquisitionActions of
    Just actionGID -> do
      -- Get the location action from the action map
      actionMap <- asks (_acquisitionActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Just (AcquisitionActionF locationAction) -> do
          -- Execute location action (removes from location)
          pure $ Left $ locationAction loc (ActionEffectMap mempty) avp
        Just (RemovedFromF locationRemovedAction) -> do
          -- Execute location removed-from action
          locationRemovedAction loc avp
        Just (AcquiredFromF locationAcquiredAction) -> do
          -- Execute location acquired-from action
          locationAcquiredAction loc avp
        _ -> pure $ Left $ modifyNarration $ updateActionConsequence "No location-specific action"
    Nothing -> pure $ Right $ modifyNarration $ updateActionConsequence "Location allows general acquisition"

executeObjectGet :: Location
                 -> ActionEffectMap
                 -> ConsumptionVerbPhrase
                 -> GameComputation Identity (Either (GameComputation Identity ()) (GameComputation Identity ()))
executeObjectGet loc actionEffectMap cvp = do
  let (objectPhrase, nounKey) = parseAcquisitionPhrase cvp

  -- Find the object in the current location
  case Data.Map.Strict.lookup nounKey loc._objectSemanticMap of
    Just objSet | not (Data.Set.null objSet) -> do
      let oid = Data.Set.elemAt 0 objSet
      obj <- getObjectM oid

      -- Check if object has acquisition actions
      let objectAcquisitionActions = _acquisitionActionManagement (_objectActionManagement obj)

      case Data.Map.Strict.lookup avp objectAcquisitionActions of
        Just objectActionGID -> do
          -- Get the object action from the action map
          actionMap <- asks (_acquisitionActionMap . _actionMaps)
          case Data.Map.Strict.lookup objectActionGID actionMap of
            Just (AcquisitionActionF objectAction) -> do
              -- Execute object action
              pure $ Left $ objectAction loc actionEffectMap avp
            Just (AcquiredFromF objectAcquiredAction) -> do
              -- Execute acquired-from action
              objectAcquiredAction loc avp
            Just (RemovedFromF objectRemovedAction) -> do
              -- Execute removed-from action
              objectRemovedAction loc avp
            Nothing -> pure $ Left $ modifyNarration $ updateActionConsequence "Object action not found in action map"
        Nothing ->
          -- No object-specific action, allow general acquisition
          pure $ Right $ modifyNarration $ updateActionConsequence "Object allows general acquisition"
    _ -> pure $ Left $ modifyNarration $ updateActionConsequence "You don't see that here."
-}
