module Build.GameState where

import           Build.GameStateGeneration.BedroomWorldDSL       (bedroomWorldDSL)
import           Build.GameStateGeneration.Defaults              (defaultNarration,
                                                                  defaultPlayer,
                                                                  defaultWorld)
import           Build.GameStateGeneration.EDSL.GameStateBuilder (initialBuilderState,
                                                                  interpretDSL,
                                                                  runWorldBuilder)
import           Build.Identifiers.Actions                       (acquisitionActionMap,
                                                                  consumptionActionMap,
                                                                  directionalStimulusActionMap,
                                                                  directionalStimulusContainerActionMap,
                                                                  implicitStimulusActionMap,
                                                                  posturalActionMap,
                                                                  somaticAccessActionMap)
import qualified Data.Map.Strict
import           Evaluators.Player.General                       (eval)
import           Model.GameState                                 (ActionMaps (ActionMaps),
                                                                  Config (Config, _actionMaps),
                                                                  GameState (GameState, _actionSystemEffectKeys, _effectRegistry, _evaluation, _narration, _player, _systemEffectRegistry, _triggerRegistry),
                                                                  _world)
import           Relude.DeepSeq                                  (deepseq)
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
      , _triggerRegistry = Data.Map.Strict.empty
      , _evaluation = eval
      , _narration = defaultNarration
      , _actionSystemEffectKeys  = mempty
      }

-- Config remains the same
config :: Config
config = Config
  { _actionMaps = actionMaps
  }
  where
    actionMaps :: ActionMaps
    actionMaps = ActionMaps
                   (Data.Map.Strict.keys implicitStimulusActionMap `deepseq` implicitStimulusActionMap)
                   (Data.Map.Strict.keys directionalStimulusActionMap `deepseq` directionalStimulusActionMap)
                   (Data.Map.Strict.keys directionalStimulusContainerActionMap `deepseq` directionalStimulusContainerActionMap)
                   (Data.Map.Strict.keys somaticAccessActionMap `deepseq` somaticAccessActionMap)
                   (Data.Map.Strict.keys acquisitionActionMap `deepseq` acquisitionActionMap)
                   (Data.Map.Strict.keys consumptionActionMap `deepseq` consumptionActionMap)
                   (Data.Map.Strict.keys posturalActionMap `deepseq` posturalActionMap)

