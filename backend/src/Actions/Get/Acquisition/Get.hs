{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module Actions.Get.Acquisition.Get (manageAcquisitionProcess) where

import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader.Class    (asks)
import qualified Data.Map.Strict
import qualified Data.Set
import           GameState                     (getLocationObjectIDsM,
                                                getPlayerActionsM,
                                                getPlayerLocationGID,
                                                getPlayerLocationM, getPlayerM)
import           Model.GameState               (AcquisitionActionF (AcquisitionActionF),
                                                ActionEffectKey (LocationKey),
                                                ActionKey (AcquisitionalActionKey),
                                                ActionKeyMap (_unActionKeyMap),
                                                ActionMaps (_acquisitionActionMap),
                                                Config (_actionMaps),
                                                GameComputation,
                                                Player (_actionKeyMap, _location),
                                                PlayerActions (_acquisitionActions))
import           Model.GID                     (GID)
import           Model.Parser.Atomics.Verbs    (AcquisitionVerb)
import           Model.Parser.Composites.Nouns (ObjectPhrase)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase (AcquisitionVerbPhrase, SimpleAcquisitionVerbPhrase))

manageAcquisitionProcess :: AcquisitionVerbPhrase
                                -> GameComputation Identity ()
manageAcquisitionProcess avp = do
  availableActions <- _acquisitionActions <$> getPlayerActionsM
  case Data.Map.Strict.lookup avp availableActions of
    Nothing -> error "Programmer Error: No directional stimulus action found for verb: "
    Just (actionGID :: GID AcquisitionActionF) -> do
      actionMap <- asks (_acquisitionActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No directional stimulus action found for GID: "
        Just (AcquisitionActionF actionFunc) -> do
          actionKeyMap <- _unActionKeyMap . _actionKeyMap <$> getPlayerM
          case Data.Map.Strict.lookup (actionKey actionGID) actionKeyMap of
            Nothing -> error $ "Programmer Error: No action key found for GID: "
            Just actionEffectMap -> do
              loc <- getPlayerLocationM
              actionFunc loc actionEffectMap avp
        Just _ -> error "Programmer Error: Expected AcquisitionActionF but got something else"
actionKey :: GID AcquisitionActionF -> ActionKey
actionKey = AcquisitionalActionKey
