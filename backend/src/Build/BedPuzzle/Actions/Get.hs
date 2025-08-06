{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Get (get,getDenied) where
import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader          (asks)
import           Control.Monad.State           (gets, modify')
import qualified Data.Map.Strict
import           Data.Set                      (elemAt, null, toList)
import           Data.Text                     (Text)
import           GameState                     (getObjectM, modifyNarration,
                                                parseAcquisitionPhrase)
import           Model.GameState               (AcquisitionActionF (AcquiredFromF, AcquisitionActionF, RemovedFromF),
                                                ActionEffectKey (ObjectKey),
                                                ActionEffectMap (ActionEffectMap, _actionEffectMap),
                                                ActionKeyMap (ActionKeyMap, _unActionKeyMap),
                                                ActionManagement (_acquisitionActionManagement, _directionalStimulusActionManagement, _implicitStimulusActionManagement, _somaticStimulusActionManagement),
                                                ActionMaps (_acquisitionActionMap),
                                                Config (_actionMaps),
                                                Effect (AcquisitionEffect),
                                                GameComputation,
                                                GameState (_player),
                                                Location (_locationActionManagement, _objectSemanticMap),
                                                Object (_objectActionManagement),
                                                Player (_actionKeyMap, _playerActions),
                                                PlayerActions (_acquisitionActions),
                                                updateActionConsequence)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase (AcquisitionVerbPhrase))

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

          -- Get the player's action key map
          player <- gets _player
          let ActionKeyMap actionKeyMap = _actionKeyMap player
              objectEffectKey = ObjectKey oid

          -- Look for acquisition effects for this object
          case Data.Map.Strict.lookup objectEffectKey (_actionEffectMap actionEffectMap) of
            Just effects -> mapM_ (processEffect avp) (Data.Set.toList effects)
            Nothing      -> pure ()
        _ -> pure ()

    processEffect :: AcquisitionVerbPhrase -> Effect -> GameComputation Identity ()
    processEffect avp (AcquisitionEffect _ newActionGID) = do
      -- Update the player's acquisition actions to use the new action for this phrase
      modify' $ \gs ->
        let player = gs._player
            playerActions = _playerActions player
            acquisitionActions = _acquisitionActions playerActions
            updatedAcquisitionActions = Data.Map.Strict.insert avp newActionGID acquisitionActions
            updatedPlayerActions = playerActions { _acquisitionActions = updatedAcquisitionActions }
            updatedPlayer = player { _playerActions = updatedPlayerActions }
        in gs { _player = updatedPlayer }
    processEffect _ _ = pure () -- Ignore other effects

executeLocationGet :: Location
                        -> AcquisitionVerbPhrase
                        -> GameComputation Identity (Either (GameComputation Identity ()) (GameComputation Identity ()))
executeLocationGet loc avp = do
  -- Look up location's acquisition actions
  let locationAcquisitionActions = _acquisitionActionManagement (_locationActionManagement loc)

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
                 -> AcquisitionVerbPhrase
                 -> GameComputation Identity (Either (GameComputation Identity ()) (GameComputation Identity ()))
executeObjectGet loc actionEffectMap avp = do
  let (objectPhrase, nounKey) = parseAcquisitionPhrase avp

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
