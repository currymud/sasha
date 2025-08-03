{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module Actions.Get.Acquisition.Get (manageAcquisitionProcess) where

import           Control.Monad.Identity     (Identity)
import           Control.Monad.Reader.Class (asks)
import qualified Data.Map.Strict
import qualified Data.Set
import           GameState                  (getLocationObjectIDsM,
                                             getPlayerActionsM, getPlayerM)
import           Model.GameState            (AcquisitionActionF (AcquisitionActionF),
                                             ActionEffectKey (LocationKey),
                                             ActionKey (AcquisitionalActionKey),
                                             ActionKeyMap (_unActionKeyMap),
                                             ActionMaps (_acquisitionActionMap),
                                             Config (_actionMaps),
                                             GameComputation,
                                             Player (_actionKeyMap, _location),
                                             PlayerActions (_acquisitionActions))
import           Model.GID                  (GID)
import           Model.Parser.Atomics.Verbs (AcquisitionVerb)

manageAcquisitionProcess :: AcquisitionVerb
                                -> GameComputation Identity ()
manageAcquisitionProcess av = do
  availableActions <- _acquisitionActions <$> getPlayerActionsM
  case Data.Map.Strict.lookup av availableActions of
    Nothing -> error "Programmer Error: No directional stimulus action found for verb: "
    Just (actionGID :: GID AcquisitionActionF) -> do
      actionMap <- asks (_acquisitionActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No directional stimulus action found for GID: "
        Just (AcquisitionActionF actionFunc) -> do
          actionKeyMap <- _unActionKeyMap . _actionKeyMap <$> getPlayerM
          case Data.Map.Strict.lookup actionKey actionKeyMap of
            Nothing -> error $ "Programmer Error: No action key found for GID: "
            Just actionEffectMap -> do
              lid <- _location <$> getPlayerM
              objectActionKeys <- getLocationObjectIDsM lid
              actionFunc (Data.Set.insert (LocationKey lid) objectActionKeys) actionEffectMap
          where
            actionKey :: ActionKey
            actionKey = AcquisitionalActionKey actionGID
