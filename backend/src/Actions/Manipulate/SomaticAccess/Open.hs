{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module Actions.Manipulate.SomaticAccess.Open (manageSomaticAccessProcess) where

import           Control.Monad.Identity     (Identity)
import           Control.Monad.Reader.Class (asks)
import qualified Data.Map.Strict
import qualified Data.Set
import           GameState                  (getLocationObjectIDsM, getPlayerM)
import           GameState.ActionManagement (lookupSomaticAccess,
                                             processEffectsFromRegistry)
import           GameState.EffectRegistry   (lookupActionEffectsInRegistry)
import           Model.GameState            (ActionEffectKey (LocationKey, PlayerKey),
                                             ActionEffectMap (ActionEffectMap),
                                             ActionKey (SomaticAccessActionKey),
                                             ActionMaps (_somaticStimulusActionMap),
                                             Config (_actionMaps, _systemEffectMap),
                                             GameComputation,
                                             Player (_location, _playerActions),
                                             SomaticAccessActionF (SomaticAccessActionF),
                                             SystemEffectConfig (_systemEffect))
import           Model.Parser.Atomics.Verbs (SomaticAccessVerb)


manageSomaticAccessProcess :: SomaticAccessVerb -> GameComputation Identity ()
manageSomaticAccessProcess sav = do
  availableActions <- _playerActions <$> getPlayerM
  case lookupSomaticAccess sav availableActions of
    Nothing -> error "Programmer Error: No somatic access action found for verb: "
    Just actionGID -> do
      actionMap <- asks (_somaticStimulusActionMap . _actionMaps)
      systemEffectMaps <- asks _systemEffectMap
      let actionMapLookup = Data.Map.Strict.lookup actionGID actionMap
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No somatic access action found for GID: "
        Just (SomaticAccessActionF actionFunc) -> do
          let actionKey = SomaticAccessActionKey actionGID
          maybeEffectMap <- lookupActionEffectsInRegistry actionKey
          case maybeEffectMap of
            Nothing -> error $ "Programmer Error: No effects registered for somatic access action"
            Just (ActionEffectMap effectMap) -> do
              lid <- _location <$> getPlayerM
              objectActionKeys <- getLocationObjectIDsM lid
              -- Add PlayerKey entries from the effect map to actionEffectKeys
              let playerKeys = Data.Set.fromList [key | key@(PlayerKey _) <- Data.Map.Strict.keys effectMap]
                  allActionKeys = Data.Set.unions [
                    Data.Set.singleton (LocationKey lid),
                    objectActionKeys,
                    playerKeys
                    ]
              actionFunc allActionKeys (ActionEffectMap effectMap)
              -- Process effects from registry after action execution
              processEffectsFromRegistry actionKey
