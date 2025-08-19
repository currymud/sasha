module Build.GameState where

import           Actions.Percieve.Look                                (isvActionEnabled)
import           Build.GameStateGeneration.BedroomWorldDSL            (bedroomWorldDSL,
                                                                       testIsaEnabledLookGID,
                                                                       testPitchBlackFGID)
import           Build.GameStateGeneration.EDSL.GameStateBuilder      (WorldBuilder,
                                                                       initialBuilderState,
                                                                       interpretDSL,
                                                                       runWorldBuilder)
import           Build.GameStateGeneration.ObjectSpec                 (defaultPlayer,
                                                                       defaultWorld)
import           Build.Identifiers.Actions                            (acquisitionActionMap,
                                                                       consumptionActionMap,
                                                                       directionalStimulusActionMap,
                                                                       posturalActionMap,
                                                                       somaticAccessActionMap)
import qualified Data.Map.Strict
import           Data.Text                                            (Text)
import           Evaluators.Player.General                            (eval)
import           GameState                                            (modifyNarration)
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb (look)
import           Model.GameState                                      (ActionMaps (ActionMaps),
                                                                       Config (Config, _actionMaps),
                                                                       GameState (GameState, _effectRegistry, _evaluation, _narration, _player, _systemEffectRegistry),
                                                                       ImplicitStimulusActionF (ImplicitStimulusActionF),
                                                                       ImplicitStimulusActionMap,
                                                                       Narration (Narration),
                                                                       World (World, _locationMap, _objectMap, _perceptionMap, _spatialRelationshipMap),
                                                                       _world,
                                                                       updateActionConsequence)
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

testPitchBlackF :: ImplicitStimulusActionF
testPitchBlackF = ImplicitStimulusActionF $ \_ _ ->
  modifyNarration $ updateActionConsequence "TEST: It's pitch black, you can't see a thing."

testIsaEnabledLook :: ImplicitStimulusActionF
testIsaEnabledLook = isvActionEnabled Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb.look

testImplicitStimulusActionMap :: ImplicitStimulusActionMap
testImplicitStimulusActionMap = Data.Map.Strict.fromList
  [ (testPitchBlackFGID, testPitchBlackF)
  , (testIsaEnabledLookGID, testIsaEnabledLook)
  ]

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
                   testImplicitStimulusActionMap
                   directionalStimulusActionMap
                   somaticAccessActionMap
                   acquisitionActionMap
                   consumptionActionMap
                   posturalActionMap
