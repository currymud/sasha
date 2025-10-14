module Examples.Initialize (gameState,config) where

import qualified Data.Map.Strict
import           EDSL.GameBuilder          (WorldBuilderResult (resultActionMaps, resultGameState),
                                            initialBuilderState, interpretDSL,
                                            runWorldBuilderWithMaps)
import           Evaluators.Player.General (eval)
import           Examples.Defaults         (defaultNarration, defaultPlayer,
                                            defaultWorld)
import           Examples.SashaDemo        (sashaBedroomDemo)
import           Model.Core                (ActionMaps (ActionMaps, _agentAcquisitionActionMap, _objectAcquisitionActionMap, _containerAcquisitionActionMap, _locationAcquisitionActionMap, _consumptionActionMap, _containerAccessActionMap, _directionalStimulusActionMap, _directionalStimulusContainerActionMap, _implicitStimulusActionMap, _posturalActionMap, _somaticStimulusActionMap),
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
      , _agentAcquisitionActionMap = Data.Map.Strict.keys (_agentAcquisitionActionMap maps) `deepseq` _agentAcquisitionActionMap maps
      , _objectAcquisitionActionMap = Data.Map.Strict.keys (_objectAcquisitionActionMap maps) `deepseq` _objectAcquisitionActionMap maps
      , _containerAcquisitionActionMap = Data.Map.Strict.keys (_containerAcquisitionActionMap maps) `deepseq` _containerAcquisitionActionMap maps
      , _locationAcquisitionActionMap = Data.Map.Strict.keys (_locationAcquisitionActionMap maps) `deepseq` _locationAcquisitionActionMap maps
      , _consumptionActionMap = Data.Map.Strict.keys (_consumptionActionMap maps) `deepseq` _consumptionActionMap maps
      , _posturalActionMap = Data.Map.Strict.keys (_posturalActionMap maps) `deepseq` _posturalActionMap maps
      , _containerAccessActionMap = Data.Map.Strict.keys (_containerAccessActionMap maps) `deepseq` _containerAccessActionMap maps
      }
