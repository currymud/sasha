{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module ActionDiscovery.Consume (manageConsumptionProcess) where

import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader.Class    (asks)
import qualified Data.Map.Strict
import qualified Data.Set
import           GameState                     (getPlayerLocationM, getPlayerM,
                                                parseConsumptionPhrase)
import           Model.Core                    (ActionEffectKey (ConsumptionActionKey),
                                                ActionMaps (_consumptionActionMap),
                                                Config (_actionMaps),
                                                ConsumptionActionF (CannotConsumeF, PlayerConsumptionActionF),
                                                GameComputation,
                                                Location (_objectSemanticMap),
                                                Player (_playerActions))

import           GameState.ActionManagement    (lookupConsumption)
import           Model.Parser.Composites.Verbs (ConsumptionVerbPhrase (ConsumptionVerbPhrase))

manageConsumptionProcess :: ConsumptionVerbPhrase -> GameComputation Identity ()
manageConsumptionProcess cvp@(ConsumptionVerbPhrase verb _)  = do
  availableActions <- _playerActions <$> getPlayerM
  case lookupConsumption verb availableActions of
    Nothing -> error "Programmer Error: No consumption action found for phrase"
    Just actionGID -> do
      actionMap <- asks (_consumptionActionMap . _actionMaps)
      let actionEffectKey = ConsumptionActionKey actionGID
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No consumption action found for GID"
        Just (PlayerConsumptionActionF actionFunc) -> do
          -- Build actionEffectKeys for the action function
          let (consumablePhrase, nounKey) = parseConsumptionPhrase cvp
          location <- getPlayerLocationM
          case Data.Map.Strict.lookup nounKey location._objectSemanticMap of
            Just objSet | not (Data.Set.null objSet) -> do
              let targetObjectGID = Data.Set.elemAt 0 objSet
              actionFunc actionEffectKey targetObjectGID cvp
            _ -> error $ "Target object not found for consumption: " <> show nounKey
        Just (CannotConsumeF actionF) -> actionF actionEffectKey
