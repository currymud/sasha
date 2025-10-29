module Examples.Initialize (gameState,config) where

import qualified Data.Map.Strict
import           EDSL.GameBuilder          (WorldBuilderResult (resultActionMaps, resultGameState),
                                            initialBuilderState, interpretDSL,
                                            runWorldBuilderWithMaps)
import           Evaluators.Player.General (eval)
import           Examples.Defaults         (defaultNarration, defaultPlayer,
                                            defaultWorld)
import           Examples.SashaDemo        (sashaBedroomDemo)
import           Model.Core                (ActionMaps (ActionMaps, _agentAcquisitionActionMap, _agentContainerAccessActionMap, _agentDirectionalStimulusActionMap, _agentDirectionalStimulusContainerActionMap, _agentImplicitStimulusActionMap, _consumptionActionMap, _containerAccessActionMap, _containerAcquisitionActionMap, _containerDirectionalStimulusContainerActionMap, _instrumentContainerAccessActionMap, _locationAcquisitionActionMap, _locationContainerAccessActionMap, _locationDirectionalStimulusActionMap, _locationDirectionalStimulusContainerActionMap, _locationImplicitStimulusActionMap, _objectAcquisitionActionMap, _objectContainerAccessActionMap, _objectDirectionalStimulusActionMap, _posturalActionMap, _somaticStimulusActionMap),
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
      { _agentImplicitStimulusActionMap = Data.Map.Strict.keys (_agentImplicitStimulusActionMap maps) `deepseq` _agentImplicitStimulusActionMap maps
      , _locationImplicitStimulusActionMap = Data.Map.Strict.keys (_locationImplicitStimulusActionMap maps) `deepseq` _locationImplicitStimulusActionMap maps
      , _agentDirectionalStimulusActionMap = Data.Map.Strict.keys (_agentDirectionalStimulusActionMap maps) `deepseq` _agentDirectionalStimulusActionMap maps
      , _objectDirectionalStimulusActionMap = Data.Map.Strict.keys (_objectDirectionalStimulusActionMap maps) `deepseq` _objectDirectionalStimulusActionMap maps
      , _locationDirectionalStimulusActionMap = Data.Map.Strict.keys (_locationDirectionalStimulusActionMap maps) `deepseq` _locationDirectionalStimulusActionMap maps
      , _somaticStimulusActionMap = Data.Map.Strict.keys (_somaticStimulusActionMap maps) `deepseq` _somaticStimulusActionMap maps
      , _agentAcquisitionActionMap = Data.Map.Strict.keys (_agentAcquisitionActionMap maps) `deepseq` _agentAcquisitionActionMap maps
      , _objectAcquisitionActionMap = Data.Map.Strict.keys (_objectAcquisitionActionMap maps) `deepseq` _objectAcquisitionActionMap maps
      , _containerAcquisitionActionMap = Data.Map.Strict.keys (_containerAcquisitionActionMap maps) `deepseq` _containerAcquisitionActionMap maps
      , _locationAcquisitionActionMap = Data.Map.Strict.keys (_locationAcquisitionActionMap maps) `deepseq` _locationAcquisitionActionMap maps
      , _consumptionActionMap = Data.Map.Strict.keys (_consumptionActionMap maps) `deepseq` _consumptionActionMap maps
      , _posturalActionMap = Data.Map.Strict.keys (_posturalActionMap maps) `deepseq` _posturalActionMap maps
      , _agentDirectionalStimulusContainerActionMap = Data.Map.Strict.keys (_agentDirectionalStimulusContainerActionMap maps) `deepseq` _agentDirectionalStimulusContainerActionMap maps
      , _containerDirectionalStimulusContainerActionMap = Data.Map.Strict.keys (_containerDirectionalStimulusContainerActionMap maps) `deepseq` _containerDirectionalStimulusContainerActionMap maps
      , _locationDirectionalStimulusContainerActionMap = Data.Map.Strict.keys (_locationDirectionalStimulusContainerActionMap maps) `deepseq` _locationDirectionalStimulusContainerActionMap maps
      , _agentContainerAccessActionMap = Data.Map.Strict.keys (_agentContainerAccessActionMap maps) `deepseq` _agentContainerAccessActionMap maps
      , _locationContainerAccessActionMap = Data.Map.Strict.keys (_locationContainerAccessActionMap maps) `deepseq` _locationContainerAccessActionMap maps
      , _objectContainerAccessActionMap = Data.Map.Strict.keys (_objectContainerAccessActionMap maps) `deepseq` _objectContainerAccessActionMap maps
      , _instrumentContainerAccessActionMap = Data.Map.Strict.keys (_instrumentContainerAccessActionMap maps) `deepseq` _instrumentContainerAccessActionMap maps
      , _containerAccessActionMap = Data.Map.Strict.keys (_containerAccessActionMap maps) `deepseq` _containerAccessActionMap maps
      }
