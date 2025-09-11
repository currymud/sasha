module ActionDiscovery.Movement.Postural.SitStand  (managePosturalProcess) where

import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader.Class    (asks)
import qualified Data.Map.Strict
import qualified Data.Set
import           GameState                     (getLocationObjectIDsM,
                                                getPlayerM)
import           GameState.ActionManagement    (lookupPostural,
                                                processEffectsFromRegistry)
import           GameState.EffectRegistry      (lookupActionEffectsInRegistry)
import           Model.Core                    (ActionEffectMap (ActionEffectMap),
                                                ActionMaps (_posturalActionMap),
                                                Config (_actionMaps),
                                                EffectActionKey (PosturalActionKey),
                                                EffectTargetKey (LocationKey, PlayerKey),
                                                GameComputation,
                                                Player (_location, _playerActions),
                                                PosturalActionF (PosturalActionF))
import           Model.Parser.Composites.Verbs (PosturalVerbPhrase)

managePosturalProcess :: PosturalVerbPhrase -> GameComputation Identity ()
managePosturalProcess posturalPhrase = do
  availableActions <- _playerActions <$> getPlayerM
  case lookupPostural posturalPhrase availableActions of
    Nothing -> error "Programmer Error: No postural action found for phrase: "
    Just actionGID -> do
      actionMap <- asks (_posturalActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No postural action found for GID: "
        Just (PosturalActionF actionFunc) -> do
          let actionKey = PosturalActionKey actionGID
          maybeEffectMap <- lookupActionEffectsInRegistry actionKey
          case maybeEffectMap of
            Nothing -> error "Programmer Error: No effects registered for postural action"
            Just (ActionEffectMap effectMap) -> do
              lid <- _location <$> getPlayerM
              objectActionKeys <- getLocationObjectIDsM lid
              -- Build actionEffectKeys following existing pattern
              let locationKeys = Data.Set.insert (LocationKey lid) objectActionKeys
                  playerKeys = Data.Set.fromList [key | key@(PlayerKey _) <- Data.Map.Strict.keys effectMap]
                  allActionKeys = Data.Set.union locationKeys playerKeys
              actionFunc allActionKeys (ActionEffectMap effectMap)
              -- Process effects from registry after action execution
              processEffectsFromRegistry actionKey
