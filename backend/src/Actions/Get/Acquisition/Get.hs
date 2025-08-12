{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module Actions.Get.Acquisition.Get (manageAcquisitionProcess) where

import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader.Class    (asks)
import           Control.Monad.State           (modify')
import qualified Data.Map.Strict
import           Data.Set                      (Set)
import qualified Data.Set
import           GameState                     (getLocationObjectIDsM,
                                                getPlayerLocationGID,
                                                getPlayerLocationM, getPlayerM,
                                                modifyObjectActionManagementM)
import           GameState.ActionManagement    (lookupAcquisition)
import           GameState.Perception          (updatePerceptionMapM)
import           Model.GameState               (AcquisitionActionF (AcquiredFromF, AcquisitionActionF, RemovedFromF),
                                                ActionEffectKey (LocationKey, ObjectKey, PlayerKey),
                                                ActionEffectMap (ActionEffectMap),
                                                ActionKey (AcquisitionalActionKey),
                                                ActionKeyMap (ActionKeyMap, _unActionKeyMap),
                                                ActionManagement (AAManagementKey, DSAManagementKey),
                                                ActionManagementFunctions (ActionManagementFunctions),
                                                ActionMaps (_acquisitionActionMap),
                                                Config (_actionMaps),
                                                Effect (AcquisitionEffect, DirectionalStimulusEffect),
                                                GameComputation,
                                                Player (_actionKeyMap, _location, _playerActions),
                                                PlayerKey (PlayerKeyObject),
                                                _player)
import           Model.GID                     (GID)
import           Model.Parser.Atomics.Verbs    (AcquisitionVerb)
import           Model.Parser.Composites.Nouns (ObjectPhrase)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase (AcquisitionVerbPhrase, SimpleAcquisitionVerbPhrase))

manageAcquisitionProcess :: AcquisitionVerbPhrase -> GameComputation Identity ()
manageAcquisitionProcess avp = do
  availableActions <- _playerActions <$> getPlayerM
  case lookupAcquisition avp availableActions of
    Nothing -> error "Programmer Error: No acquisition action found for verb phrase"
    Just actionGID -> do
      actionMap <- asks (_acquisitionActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No acquisition action found for GID"
        Just (AcquisitionActionF actionFunc) -> do
          actionKeyMap <- _unActionKeyMap . _actionKeyMap <$> getPlayerM
          case Data.Map.Strict.lookup (actionKey actionGID) actionKeyMap of
            Nothing -> error $ "Programmer Error: No action key found for GID"
            Just actionEffectMap -> do
              loc <- getPlayerLocationM
              actionFunc loc actionEffectMap avp
              processAcquisitionEffects actionGID avp
        Just (RemovedFromF locationRemovedAction) -> do
          loc <- getPlayerLocationM
          result <- locationRemovedAction loc avp
          case result of
            Left failureAction -> failureAction
            Right successAction -> do
              successAction
              processAcquisitionEffects actionGID avp
        Just (AcquiredFromF locationAcquiredAction) -> do
          loc <- getPlayerLocationM
          result <- locationAcquiredAction loc avp
          case result of
            Left failureAction -> failureAction
            Right successAction -> do
              successAction
              processAcquisitionEffects actionGID avp


-- Add helper function to process effects
processAcquisitionEffects :: GID AcquisitionActionF -> AcquisitionVerbPhrase -> GameComputation Identity ()
processAcquisitionEffects actionGID avp = do
  player <- getPlayerM
  let ActionKeyMap actionKeyMap = _actionKeyMap player
      acquisitionKey = AcquisitionalActionKey actionGID

  case Data.Map.Strict.lookup acquisitionKey actionKeyMap of
    Just (ActionEffectMap effectMap) -> do
      -- Process all effects in the effect map
      mapM_ (processEffectEntry avp) (Data.Map.Strict.toList effectMap)
    Nothing -> pure () -- No effects defined

processEffectEntry :: AcquisitionVerbPhrase -> (ActionEffectKey, Set Effect) -> GameComputation Identity ()
processEffectEntry avp (effectKey, effects) = do
  mapM_ (processEffect avp effectKey) (Data.Set.toList effects)

processEffect :: AcquisitionVerbPhrase -> ActionEffectKey -> Effect -> GameComputation Identity ()
processEffect avp effectKey (DirectionalStimulusEffect verb newActionGID) = do
  case effectKey of
    ObjectKey oid -> do
      -- Update the object's directional stimulus action
      modifyObjectActionManagementM oid $ \actionMgmt ->
        let ActionManagementFunctions actionSet = actionMgmt
            -- Remove old directional stimulus actions for this verb
            filteredActions = Data.Set.filter (\case DSAManagementKey v _ -> v /= verb; _ -> True) actionSet
            -- Add new action
            updatedActions = Data.Set.insert (DSAManagementKey verb newActionGID) filteredActions
        in ActionManagementFunctions updatedActions
    _ -> pure ()
processEffect _ _ _ = pure () -- Handle other effect types as needed

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

processEffectWithKey _ _ _ = pure () -- Handle other effect types as needed


actionKey :: GID AcquisitionActionF -> ActionKey
actionKey = AcquisitionalActionKey
