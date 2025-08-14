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
  pure ()
