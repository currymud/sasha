module ActionDiscovery.Movement.Postural.SitStand  (managePosturalProcess) where

import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader.Class    (asks)
import qualified Data.Map.Strict
import           GameState                     (getPlayerM)
import           GameState.ActionManagement    (lookupPostural)
import           Model.Core                    (ActionEffectKey (PosturalActionKey),
                                                ActionMaps (_posturalActionMap),
                                                Config (_actionMaps),
                                                GameComputation,
                                                Player (_playerActions),
                                                PosturalActionF (CannotPosturalActionF, PlayerPosturalActionF))
import           Model.Parser.Composites.Verbs (PosturalVerbPhrase)

managePosturalProcess :: PosturalVerbPhrase -> GameComputation Identity ()
managePosturalProcess posturalPhrase = do
  availableActions <- _playerActions <$> getPlayerM
  case lookupPostural posturalPhrase availableActions of
    Nothing -> error "Programmer Error: No postural action found for phrase: "
    Just actionGID -> do
      actionMap <- asks (_posturalActionMap . _actionMaps)
      let actionEffectKey = PosturalActionKey actionGID
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No postural action found for GID: "
        Just (PlayerPosturalActionF actionFunc) -> do
          actionFunc actionEffectKey
        Just (CannotPosturalActionF actionFunc) -> actionFunc actionEffectKey
