module Build.GameState where

import           Build.GameStateGeneration.BedroomWorldDSL       (bedroomWorldDSL)
import           Build.GameStateGeneration.EDSL.GameStateBuilder (WorldBuilder,
                                                                  initialBuilderState,
                                                                  interpretDSL,
                                                                  runWorldBuilder)
import           Build.GameStateGeneration.ObjectSpec            (defaultPlayer,
                                                                  defaultWorld)
import           Build.Identifiers.Actions                       (acquisitionActionMap,
                                                                  consumptionActionMap,
                                                                  directionalStimulusActionMap,
                                                                  implicitStimulusActionMap,
                                                                  posturalActionMap,
                                                                  somaticAccessActionMap)
import qualified Data.Map.Strict
import           Data.Text                                       (Text)
import           Evaluators.Player.General                       (eval)
import           Model.GameState                                 (ActionMaps (ActionMaps),
                                                                  Config (Config, _actionMaps),
                                                                  GameState (GameState, _effectRegistry, _evaluation, _narration, _player, _systemEffectRegistry),
                                                                  Narration (Narration),
                                                                  World (World, _locationMap, _objectMap, _perceptionMap, _spatialRelationshipMap),
                                                                  _world)

-- Build GameState using the DSL!
gameState :: GameState
gameState = case runWorldBuilder (interpretDSL bedroomWorldDSL) (initialBuilderState defaultGameState) of
  Left err             -> error $ "Failed to build game state: " ++ show err
  Right finalGameState -> finalGameState
  where
    -- Minimal GameState to start the DSL builder
    defaultGameState = GameState
      { _world = defaultWorld
      , _player = defaultPlayer
      , _effectRegistry = Data.Map.Strict.empty
      , _systemEffectRegistry = Data.Map.Strict.empty
      , _evaluation = eval
      , _narration = defaultNarration
      }

defaultNarration :: Narration
defaultNarration = Narration ["Let's begin" :: Text] mempty
-- Config remains the same
config :: Config
config = Config
  { _actionMaps = actionMaps
  }
  where
    actionMaps :: ActionMaps
    actionMaps = ActionMaps
                   implicitStimulusActionMap
                   directionalStimulusActionMap
                   somaticAccessActionMap
                   acquisitionActionMap
                   consumptionActionMap
                   posturalActionMap
