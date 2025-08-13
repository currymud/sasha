{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module Actions.Get.Acquisition.Get (manageAcquisitionProcess) where

import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader.Class    (asks)
import qualified Data.Map.Strict
import           GameState                     (getPlayerLocationM, getPlayerM)
import           GameState.ActionManagement    (lookupAcquisition,
                                                processEffectsFromRegistry)
import           GameState.EffectRegistry      (lookupEffectsInRegistry)
import           Model.GameState               (AcquisitionActionF (AcquiredFromF, AcquisitionActionF, RemovedFromF),
                                                ActionKey (AcquisitionalActionKey),
                                                ActionMaps (_acquisitionActionMap),
                                                Config (_actionMaps),
                                                GameComputation,
                                                Player (_location, _playerActions),
                                                _player)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase (AcquisitionVerbPhrase))

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
          let actionKey = AcquisitionalActionKey actionGID
          maybeEffectMap <- lookupEffectsInRegistry actionKey
          case maybeEffectMap of
            Nothing -> error "Programmer Error: No effects registered for acquisition action"
            Just effectMap -> do
              loc <- getPlayerLocationM
              actionFunc loc effectMap avp
              -- Process effects from registry after action execution
              processEffectsFromRegistry actionKey
        Just (RemovedFromF locationRemovedAction) -> do
          loc <- getPlayerLocationM
          result <- locationRemovedAction loc avp
          case result of
            Left failureAction -> failureAction
            Right successAction -> do
              successAction
              -- Process effects from registry after successful action
              let actionKey = AcquisitionalActionKey actionGID
              processEffectsFromRegistry actionKey
        Just (AcquiredFromF locationAcquiredAction) -> do
          loc <- getPlayerLocationM
          result <- locationAcquiredAction loc avp
          case result of
            Left failureAction -> failureAction
            Right successAction -> do
              successAction
              -- Process effects from registry after successful action
              let actionKey = AcquisitionalActionKey actionGID
              processEffectsFromRegistry actionKey
