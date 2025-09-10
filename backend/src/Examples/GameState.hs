module Examples.GameState (gameState,config) where

import qualified Data.Map.Strict
import           Debug.Trace               (trace)
import           EDSL.GameStateBuilder     (WorldBuilderResult (resultActionMaps, resultGameState),
                                            initialBuilderState, interpretDSL,
                                            runWorldBuilderWithMaps)
import           Evaluators.Player.General (eval)
import           Examples.Defaults         (defaultNarration, defaultPlayer,
                                            defaultWorld)
import           Examples.SashaDemo        (sashaBedroomDemo)
import           Model.Core                (ActionMaps (ActionMaps, _acquisitionActionMap, _consumptionActionMap, _containerAccessActionMap, _directionalStimulusActionMap, _directionalStimulusContainerActionMap, _implicitStimulusActionMap, _posturalActionMap, _somaticStimulusActionMap),
                                            Config (Config, _actionMaps),
                                            Evaluator (Evaluator),
                                            GameState (GameState, _actionSystemEffectKeys, _effectRegistry, _evaluation, _narration, _player, _systemEffectRegistry, _triggerRegistry, _world),
                                            TriggerRegistry (TriggerRegistry))
import           Relude.DeepSeq            (deepseq)

buildResult :: WorldBuilderResult
buildResult = case runWorldBuilderWithMaps (interpretDSL sashaBedroomDemo) (initialBuilderState defaultGameState) of
  Left err     -> error $ "Failed to build game state: " ++ show err
  Right result -> result
  where
    defaultGameState = GameState
      { _world = defaultWorld
      , _player = defaultPlayer
      , _effectRegistry = Data.Map.Strict.empty
      , _systemEffectRegistry = Data.Map.Strict.empty
      , _triggerRegistry = TriggerRegistry Data.Map.Strict.empty
      , _evaluation = Evaluator eval
      , _narration = defaultNarration
      , _actionSystemEffectKeys = mempty
      }

gameState :: GameState
gameState = resultGameState buildResult

config :: Config
config = Config { _actionMaps = forcedMaps }
  where
    maps = resultActionMaps buildResult
    forcedMaps = ActionMaps
      { _implicitStimulusActionMap = Data.Map.Strict.keys (_implicitStimulusActionMap maps) `deepseq` _implicitStimulusActionMap maps
      , _directionalStimulusActionMap = Data.Map.Strict.keys (_directionalStimulusActionMap maps) `deepseq` _directionalStimulusActionMap maps
      , _directionalStimulusContainerActionMap = Data.Map.Strict.keys (_directionalStimulusContainerActionMap maps) `deepseq` _directionalStimulusContainerActionMap maps
      , _somaticStimulusActionMap = Data.Map.Strict.keys (_somaticStimulusActionMap maps) `deepseq` _somaticStimulusActionMap maps
      , _acquisitionActionMap = Data.Map.Strict.keys (_acquisitionActionMap maps) `deepseq` _acquisitionActionMap maps
      , _consumptionActionMap = Data.Map.Strict.keys (_consumptionActionMap maps) `deepseq` _consumptionActionMap maps
      , _posturalActionMap = Data.Map.Strict.keys (_posturalActionMap maps) `deepseq` _posturalActionMap maps
      , _containerAccessActionMap = Data.Map.Strict.keys (_containerAccessActionMap maps) `deepseq` _containerAccessActionMap maps
      }

-- Build GameState using the DSL!
  {-
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
      { implicitStimulusActionMap = Data.Map.Strict.empty
      , directionalStimulusActionMap = Data.Map.Strict.empty
      , directionalStimulusContainerActionMap = Data.Map.Strict.empty
      , somaticAccessActionMap = Data.Map.Strict.empty
      , acquisitionActionMap = Data.Map.Strict.empty
      , consumptionActionMap = Data.Map.Strict.empty
      , posturalActionMap = Data.Map.Strict.empty
      }
      {-
                   (Data.Map.Strict.keys implicitStimulusActionMap `deepseq` implicitStimulusActionMap)
                   (Data.Map.Strict.keys directionalStimulusActionMap `deepseq` directionalStimulusActionMap)
                   (Data.Map.Strict.keys directionalStimulusContainerActionMap `deepseq` directionalStimulusContainerActionMap)
                   (Data.Map.Strict.keys somaticAccessActionMap `deepseq` somaticAccessActionMap)
                   (Data.Map.Strict.keys acquisitionActionMap `deepseq` acquisitionActionMap)
                   (Data.Map.Strict.keys consumptionActionMap `deepseq` consumptionActionMap)
                   (Data.Map.Strict.keys posturalActionMap `deepseq` posturalActionMap)
-}
-}
