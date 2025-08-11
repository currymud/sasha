{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module Actions.Consume (manageConsumptionProcess) where

import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader.Class    (asks)
import qualified Data.Map.Strict
import qualified Data.Set
import           GameState                     (getPlayerLocationM, getPlayerM,
                                                parseConsumptionPhrase)
import           Model.GameState               (ActionEffectMap (ActionEffectMap),
                                                ActionKey (ConsumptionActionKey),
                                                ActionKeyMap (_unActionKeyMap),
                                                ActionMaps (_consumptionActionMap),
                                                Config (_actionMaps),
                                                ConsumptionActionF (ConsumptionActionF),
                                                GameComputation,
                                                Location (_objectSemanticMap),
                                                Player (_actionKeyMap, _playerActions))

import           Error                         (throwMaybeM)
import           GameState.ActionManagement    (lookupConsumption)
import           Model.Parser.Composites.Verbs (ConsumptionVerbPhrase)

manageConsumptionProcess :: ConsumptionVerbPhrase -> GameComputation Identity ()
manageConsumptionProcess cvp = do
  availableActions <- _playerActions <$> getPlayerM
  actionMap <- asks (_consumptionActionMap . _actionMaps)
  location <- getPlayerLocationM
  actionKeyMap <- _unActionKeyMap . _actionKeyMap <$> getPlayerM
  let (_consumablePhrase, nounKey) = parseConsumptionPhrase cvp

      resolveConsumption = do
        actionGID <- lookupConsumption cvp availableActions
        ConsumptionActionF actionFunc <- Data.Map.Strict.lookup actionGID actionMap
        objSet <- Data.Map.Strict.lookup nounKey (_objectSemanticMap location)
        if Data.Set.null objSet
          then Nothing
          else Just (actionFunc, actionGID, Data.Set.elemAt 0 objSet)

  (actionFunc, actionGID, targetObjectGID) <- throwMaybeM "Consumption processing failed: missing action, object, or location data" resolveConsumption
  let effectMap = Data.Map.Strict.findWithDefault (ActionEffectMap mempty) (ConsumptionActionKey actionGID) actionKeyMap
  actionFunc targetObjectGID effectMap cvp
