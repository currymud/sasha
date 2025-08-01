{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module Actions.Manipulate.SomaticAccess.Open (manageSomaticAccessProcess) where

import           Control.Monad.Identity     (Identity)
import           Control.Monad.Reader.Class (asks)
import qualified Data.Map.Strict
import           GameState                  (getPlayerActionsM, getPlayerM)
import           Model.GameState            (ActionEffectMap (ActionEffectMap),
                                             ActionKey (SomaticAccessActionKey),
                                             ActionKeyMap (ActionKeyMap, _unActionKeyMap),
                                             ActionMaps (_somaticStimulusActionMap),
                                             Config (_actionMaps),
                                             GameComputation,
                                             Player (_actionKeyMap),
                                             PlayerActions (_somaticStimulusActions),
                                             SomaticAccessActionF (SomaticAccessActionF))
import           Model.GID                  (GID)
import           Model.Parser.Atomics.Verbs (SomaticAccessVerb)

manageSomaticAccessProcess :: SomaticAccessVerb
                                -> GameComputation Identity ()
manageSomaticAccessProcess sav = do
  availableActions <- _somaticStimulusActions <$> getPlayerActionsM
  case Data.Map.Strict.lookup sav availableActions of
    Nothing -> error "Programmer Error: No directional stimulus action found for verb: "
    Just (actionGID :: GID SomaticAccessActionF) -> do
      actionMap <- asks (_somaticStimulusActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error $ "Programmer Error: No directional stimulus action found for GID: "
        Just (SomaticAccessActionF actionFunc) -> do
          actionKeyMap <- _unActionKeyMap . _actionKeyMap <$> getPlayerM
          case Data.Map.Strict.lookup actionKey actionKeyMap of
            Nothing -> error $ "Programmer Error: No action key found for GID: "
            Just actionEffectMap -> actionFunc actionEffectMap
          where
            actionKey :: ActionKey
            actionKey = SomaticAccessActionKey actionGID
