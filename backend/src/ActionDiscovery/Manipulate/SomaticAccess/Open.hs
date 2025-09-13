module ActionDiscovery.Manipulate.SomaticAccess.Open where
import           Control.Monad.Identity     (Identity)
import           Control.Monad.Reader       (asks)
import           Control.Monad.State        (get)
import           Control.Monad.State.Strict (gets)
import qualified Data.Map.Strict
import qualified Data.Set
import           GameState                  (getLocationObjectIDsM, getPlayerM)
import           GameState.ActionManagement (lookupSomaticAccess,
                                             processEffectsFromRegistry)
import           GameState.EffectRegistry   (lookupActionEffectsInRegistry)
import           Model.Core                 (TargetEffectKey (LocationKey, PlayerKey),
                                             ActionEffectMap (ActionEffectMap),
                                             ActionMaps (_somaticStimulusActionMap),
                                             Config (_actionMaps),
                                             ActionEffectKey (SomaticAccessActionKey),
                                             GameComputation, GameState (..),
                                             Player (_location, _playerActions),
                                             SomaticAccessActionF (CannotSomaticAccessF, PlayerSomaticAccessActionF))
import           Model.Parser.Atomics.Verbs (SomaticAccessVerb)

manageSomaticAccessProcess :: SomaticAccessVerb -> GameComputation Identity ()
manageSomaticAccessProcess sav = do
  availableActions <- _playerActions <$> getPlayerM
  case lookupSomaticAccess sav availableActions of
    Nothing -> error "Programmer Error: No somatic access action found for verb: "
    Just actionGID -> do
      actionMap <- asks (_somaticStimulusActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No somatic access action found for GID: "
        Just (PlayerSomaticAccessActionF actionFunc) -> do
          let actionKey = SomaticAccessActionKey actionGID
          maybeEffectMap <- lookupActionEffectsInRegistry actionKey
          case maybeEffectMap of
            Nothing -> error "Programmer Error: No effects registered for somatic access action"
            Just (ActionEffectMap effectMap) -> do
              lid <- _location <$> getPlayerM
              objectActionKeys <- getLocationObjectIDsM lid

              -- Get SystemEffectKeys from GameState
              systemEffectKeysRegistry <- gets _actionSystemEffectKeys
              let systemEffectKeysForAction = Data.Map.Strict.findWithDefault [] actionKey systemEffectKeysRegistry

              -- Get SystemEffectRegistry from GameState
              systemEffectRegistry <- gets _systemEffectRegistry

              let playerKeys = Data.Set.fromList [key | key@(PlayerKey _) <- Data.Map.Strict.keys effectMap]
                  allActionKeys = Data.Set.unions [
                    Data.Set.singleton (LocationKey lid),
                    objectActionKeys,
                    playerKeys
                    ]

              actionFunc allActionKeys systemEffectKeysForAction (ActionEffectMap effectMap) systemEffectRegistry
              processEffectsFromRegistry actionKey
        Just (CannotSomaticAccessF actionFunc) -> actionFunc
