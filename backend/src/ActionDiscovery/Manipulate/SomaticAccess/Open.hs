module ActionDiscovery.Manipulate.SomaticAccess.Open where
import           Control.Monad.Identity     (Identity)
import           Control.Monad.Reader       (asks)
import qualified Data.Map.Strict
import           GameState                  (getPlayerM)
import           GameState.ActionManagement (lookupSomaticAccess)
import           Model.Core                 (ActionEffectKey (AgentSomaticAccessActionKey),
                                             ActionMaps (_somaticStimulusActionMap),
                                             AgentSomaticAccessActionF (AgentSomaticAccessActionF),
                                             Config (_actionMaps),
                                             GameComputation,
                                             Player (_playerActions))
import           Model.Parser.Atomics.Verbs (SomaticAccessVerb)

manageSomaticAccessProcess :: SomaticAccessVerb -> GameComputation Identity ()
manageSomaticAccessProcess sav = do
  availableActions <- _playerActions <$> getPlayerM
  case lookupSomaticAccess sav availableActions of
    Nothing -> error "Programmer Error: No somatic access action found for verb: "
    Just actionGID -> do
      let actionEffectKey = AgentSomaticAccessActionKey actionGID
      actionMap <- asks (_somaticStimulusActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No somatic access action found for GID: "
        Just (AgentSomaticAccessActionF actionFunc) -> do
          actionFunc actionEffectKey
