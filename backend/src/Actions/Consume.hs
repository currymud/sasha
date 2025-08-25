{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module Actions.Consume (manageConsumptionProcess) where

import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader.Class    (asks)
import qualified Data.Map.Strict
import qualified Data.Set
import           GameState                     (getLocationObjectIDsM,
                                                getPlayerLocationM, getPlayerM,
                                                parseConsumptionPhrase)
import           Model.GameState               (ActionEffectKey (LocationKey, PlayerKey),
                                                ActionEffectMap (ActionEffectMap),
                                                ActionKey (RegularEffectKey),
                                                ActionMaps (_consumptionActionMap),
                                                Config (_actionMaps),
                                                ConsumptionActionF (ConsumptionActionF),
                                                EffectActionKey (ConsumptionActionKey),
                                                GameComputation,
                                                Location (_objectSemanticMap),
                                                Player (_location, _playerActions))

import           GameState.ActionManagement    (lookupConsumption,
                                                processEffectsFromRegistry)
import           GameState.EffectRegistry      (lookupActionEffectsInRegistry)
import           Model.Parser.Composites.Verbs (ConsumptionVerbPhrase)

manageConsumptionProcess :: ConsumptionVerbPhrase -> GameComputation Identity ()
manageConsumptionProcess cvp = do
  availableActions <- _playerActions <$> getPlayerM
  case lookupConsumption cvp availableActions of
    Nothing -> error "Programmer Error: No consumption action found for phrase"
    Just actionGID -> do
      actionMap <- asks (_consumptionActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No consumption action found for GID"
        Just (ConsumptionActionF actionFunc) -> do
          lid <- _location <$> getPlayerM
          objectActionKeys <- getLocationObjectIDsM lid

          -- Build actionEffectKeys for the action function
          let actionKey = RegularEffectKey (ConsumptionActionKey actionGID)
          maybeEffectMap <- lookupActionEffectsInRegistry actionKey
          case maybeEffectMap of
            Nothing -> error "Programmer Error: No effects registered for consumption action"
            Just (ActionEffectMap effectMap) -> do
              let locationKeys = Data.Set.singleton (LocationKey lid)
                  playerKeys = Data.Set.fromList [key | key@(PlayerKey _) <- Data.Map.Strict.keys effectMap]
                  allActionKeys = Data.Set.unions [locationKeys, objectActionKeys, playerKeys]

              -- Parse consumption phrase to find target object
              let (consumablePhrase, nounKey) = parseConsumptionPhrase cvp
              location <- getPlayerLocationM
              case Data.Map.Strict.lookup nounKey location._objectSemanticMap of
                Just objSet | not (Data.Set.null objSet) -> do
                  let targetObjectGID = Data.Set.elemAt 0 objSet
                  actionFunc targetObjectGID allActionKeys (ActionEffectMap effectMap) cvp
                  -- Process effects from registry after action execution
                  processEffectsFromRegistry actionKey
                _ -> error $ "Target object not found for consumption: " <> show nounKey
