{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module Actions.Consume () where

import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader.Class    (asks)
import qualified Data.Map.Strict
import           Data.Set                      (Set)
import qualified Data.Set
import           GameState                     (getPlayerActionsM,
                                                getPlayerLocationM, getPlayerM,
                                                modifyObjectActionManagementM)
import           Model.GameState               (AcquisitionActionF (AcquisitionActionF),
                                                ActionEffectKey (ObjectKey),
                                                ActionEffectMap (ActionEffectMap),
                                                ActionKey (AcquisitionalActionKey),
                                                ActionKeyMap (ActionKeyMap, _unActionKeyMap),
                                                ActionMaps (_acquisitionActionMap),
                                                Config (_actionMaps),
                                                ConsumptionActionF,
                                                Effect (DirectionalStimulusEffect),
                                                GameComputation,
                                                Player (_actionKeyMap, _location),
                                                _directionalStimulusActionManagement)
import           Model.GID                     (GID)
import           Model.Parser.Composites.Verbs (ConsumptionVerbPhrase)
  {-
manageConsumptionProcess :: ConsumptionVerbPhrase -> GameComputation Identity ()
manageConsumptionProcess cvp = do
  availableActions <- _consumptionActions <$> getPlayerActionsM
  case Data.Map.Strict.lookup cvp availableActions of
    Nothing -> error "Programmer Error: No acquisition action found for verb phrase"
    Just (actionGID :: GID AcquisitionActionF) -> do
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
              -- *** ADD EFFECT PROCESSING HERE ***
              processAcquisitionEffects actionGID avp
        Just _ -> error "Programmer Error: Expected AcquisitionActionF but got something else"

-- Add helper function to process effects
processConsumptionEffects :: GID ConsumptionActionF
                               -> ConsumptionVerbPhrase
                               -> GameComputation Identity ()
processConsumptionEffects actionGID cvp = do
  player <- getPlayerM
  let ActionKeyMap actionKeyMap = _actionKeyMap player
      sitionKey = ConsumptionActionKey actionGID

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
        let directionalMap = _directionalStimulusActionManagement actionMgmt
            updatedMap = Data.Map.Strict.insert verb newActionGID directionalMap
        in actionMgmt { _directionalStimulusActionManagement = updatedMap }
    _ -> pure ()
processEffect _ _ _ = pure () -- Handle other effect types as needed

actionKey :: GID AcquisitionActionF -> ActionKey
actionKey = AcquisitionalActionKey
-}
