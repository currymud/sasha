{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module Actions.Get.Acquisition.Get (manageAcquisitionProcess) where

import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader.Class    (asks)
import qualified Data.Map.Strict
import           Data.Set                      (Set)
import qualified Data.Set
import           GameState                     (getLocationObjectIDsM,
                                                getPlayerActionsM,
                                                getPlayerLocationGID,
                                                getPlayerLocationM, getPlayerM,
                                                modifyObjectActionManagementM)
import           Model.GameState               (AcquisitionActionF (AcquisitionActionF),
                                                ActionEffectKey (LocationKey, ObjectKey),
                                                ActionEffectMap (ActionEffectMap),
                                                ActionKey (AcquisitionalActionKey),
                                                ActionKeyMap (ActionKeyMap, _unActionKeyMap),
                                                ActionMaps (_acquisitionActionMap),
                                                Config (_actionMaps),
                                                Effect (DirectionalStimulusEffect),
                                                GameComputation,
                                                Player (_actionKeyMap, _location),
                                                PlayerActions (_acquisitionActions),
                                                _directionalStimulusActionManagement)
import           Model.GID                     (GID)
import           Model.Parser.Atomics.Verbs    (AcquisitionVerb)
import           Model.Parser.Composites.Nouns (ObjectPhrase)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase (AcquisitionVerbPhrase, SimpleAcquisitionVerbPhrase))

manageAcquisitionProcess :: AcquisitionVerbPhrase -> GameComputation Identity ()
manageAcquisitionProcess avp = do
  availableActions <- _acquisitionActions <$> getPlayerActionsM
  case Data.Map.Strict.lookup avp availableActions of
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
        let directionalMap = _directionalStimulusActionManagement actionMgmt
            updatedMap = Data.Map.Strict.insert verb newActionGID directionalMap
        in actionMgmt { _directionalStimulusActionManagement = updatedMap }
    _ -> pure ()
processEffect _ _ _ = pure () -- Handle other effect types as needed

actionKey :: GID AcquisitionActionF -> ActionKey
actionKey = AcquisitionalActionKey
