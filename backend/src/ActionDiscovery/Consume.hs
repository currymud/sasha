{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module ActionDiscovery.Consume (manageConsumptionProcess) where

import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader.Class    (asks)
import qualified Data.Map.Strict
import qualified Data.Set
import           GameState                     (getPlayerLocationGID,
                                                getPlayerLocationM, getPlayerM,
                                                getWorldM,
                                                parseConsumptionPhrase)
import           Model.Core                    (ActionEffectKey (ConsumptionActionKey),
                                                ActionMaps (_consumptionActionMap),
                                                Config (_actionMaps),
                                                ConsumptionActionF (ObjectCannotBeConsumedF, ObjectConsumedF, PlayerCannotConsumeF, PlayerConsumptionActionF),
                                                GameComputation,
                                                Location (_locationInventory, _objectSemanticMap),
                                                Player (_playerActions),
                                                World (_globalSemanticMap))

import           GameState.ActionManagement    (lookupConsumption)
import           Model.Parser.Composites.Verbs (ConsumptionVerbPhrase (ConsumptionVerbPhrase))

manageConsumptionProcess :: ConsumptionVerbPhrase -> GameComputation Identity ()
manageConsumptionProcess cvp@(ConsumptionVerbPhrase verb _)  = do
  availableActions <- _playerActions <$> getPlayerM
  case lookupConsumptionF availableActions of
    Nothing -> error "Programmer Error: No consumption action found for phrase"
    Just actionGID -> do
      actionMap <- asks (_consumptionActionMap . _actionMaps)
      let actionEffectKey = ConsumptionActionKey actionGID
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No consumption action found for GID"
        Just (PlayerConsumptionActionF actionFunc) -> do
          -- Build actionEffectKeys for the action function
          let (consumablePhrase, nounKey) = parseConsumptionPhrase cvp
          world <- getWorldM
          location <- getPlayerLocationM
          -- Check global semantic map for objects with this nounKey
          case Data.Map.Strict.lookup nounKey (_globalSemanticMap world) of
            Just objSet | not (Data.Set.null objSet) -> do
              -- Find first object that's in the current location's inventory
              let locationInventory = _locationInventory location
                  availableObjects = Data.Set.filter (`Data.Set.member` locationInventory) objSet
              if not (Data.Set.null availableObjects)
                then do
                  let targetObjectGID = Data.Set.elemAt 0 availableObjects
                  actionFunc actionEffectKey targetObjectGID cvp
                else error $ "Target object not found in this location: " <> show nounKey
            _ -> error $ "Target object not found for consumption: " <> show nounKey
        Just (ObjectConsumedF _) -> error "Programmer Error: ObjectConsumedF found in player actions"
        Just (ObjectCannotBeConsumedF _) -> error "Programmer Error: ObjectCannotBeConsumedF found in player actions"
        Just (PlayerCannotConsumeF actionF) -> actionF actionEffectKey
  where
    lookupConsumptionF = lookupConsumption verb
