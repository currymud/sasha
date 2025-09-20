module ActionDiscovery.Manipulate.SomaticAccess.Open where
import           Control.Monad.Identity     (Identity)
import           Control.Monad.Reader       (asks)
import qualified Data.Map.Strict
import           GameState                  (getPlayerM)
import           GameState.ActionManagement (lookupSomaticAccess)
import           Model.Core                 (ActionEffectKey (SomaticAccessActionKey),
                                             ActionMaps (_somaticStimulusActionMap),
                                             Config (_actionMaps),
                                             GameComputation,
                                             Player (_playerActions),
                                             SomaticAccessActionF (CannotSomaticAccessF, PlayerSomaticAccessActionF))
import           Model.Parser.Atomics.Verbs (SomaticAccessVerb)

manageSomaticAccessProcess :: SomaticAccessVerb -> GameComputation Identity ()
manageSomaticAccessProcess sav = do
  availableActions <- _playerActions <$> getPlayerM
  case lookupSomaticAccess sav availableActions of
    Nothing -> error "Programmer Error: No somatic access action found for verb: "
    Just actionGID -> do
      let actionEffectKey = SomaticAccessActionKey actionGID
      actionMap <- asks (_somaticStimulusActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No somatic access action found for GID: "
        Just (PlayerSomaticAccessActionF actionFunc) -> do
          actionFunc actionEffectKey
        Just (CannotSomaticAccessF actionFunc) -> actionFunc actionEffectKey
