module ActionDiscovery.Manipulate.SomaticAccess.Open where
import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader          (asks)
import           Control.Monad.State.Strict    (gets)
import qualified Data.Map.Strict
import qualified Data.Set
import           GameState                     (getLocationObjectIDsM,
                                                getPlayerM)
import           GameState.ActionManagement    (lookupSomaticAccess,
                                                processEffectsFromRegistry)
import           GameState.EffectRegistry      (lookupActionEffectsInRegistry)
import           Model.Core                    (ActionEffectKey (SomaticAccessActionKey),
                                                ActionEffectMap (ActionEffectMap),
                                                ActionMaps (_somaticStimulusActionMap),
                                                Config (_actionMaps),
                                                EffectTargetKey (LocationKey, PlayerKey),
                                                GameComputation, GameState (..),
                                                Player (_location, _playerActions),
                                                SomaticAccessActionF (CannotSomaticAccessF, SomaticAccessActionF))
import           Model.Parser.Composites.Verbs (SomaticStimulusVerbPhrase (SomaticStimulusVerbPhrase))

manageSomaticAccessProcess :: SomaticStimulusVerbPhrase -> GameComputation Identity ()
manageSomaticAccessProcess (SomaticStimulusVerbPhrase sav _) = do
  availableActions <- _playerActions <$> getPlayerM
  case lookupSomaticAccess sav availableActions of
    Nothing -> error "Programmer Error: No somatic access action found for verb: "
    Just actionGID -> do
      actionMap <- asks (_somaticStimulusActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No somatic access action found for GID: "
        Just (CannotSomaticAccessF actionF) -> do
          let effectKey = SomaticAccessActionKey actionGID
          actionF effectKey
        Just (SomaticAccessActionF actionF) -> do
          let effectKey = SomaticAccessActionKey actionGID
          actionF effectKey
