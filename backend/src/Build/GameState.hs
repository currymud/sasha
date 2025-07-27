module Build.GameState where
import           Build.Identifiers.Actions            (actionMap)
import           Build.Identifiers.Locations          (bedroomInBedGID)
import           Build.Identifiers.SentenceProcessing (playerProcessImplicitVerbMap,
                                                       processImplicitVerbMaps)
import           Build.World                          (world)
import           Data.Text                            (Text)
import           Evaluators.Player.General            (eval)
import           Model.GameState                      (Config (Config, _actionMap, _sentenceProcessingMaps),
                                                       GameState (GameState, _evaluation, _narration, _player, _world),
                                                       Narration (..),
                                                       Perceptables (Perceptables),
                                                       Player (Player, _location, _perceptables, _sentenceManagement),
                                                       PlayerSentenceProcessingMaps (PlayerSentenceProcessingMaps),
                                                       SentenceProcessingMaps (SentenceProcessingMaps, _processImplicitVerbMap))

initNarration :: Narration
initNarration = Narration
  { _playerAction  = [initialAction]
  , _actionConsequence = mempty
  }

initialAction :: Text
initialAction = "Finally! Let's see how look works"


gameState :: GameState
gameState = GameState
  { _world = world
  , _player = player
  , _narration = initNarration
  , _evaluation = eval
  }

config :: Config
config = Config
  { _actionMap = actionMap
  , _sentenceProcessingMaps = sentenceProcessingMaps
  }

sentenceProcessingMaps :: SentenceProcessingMaps
sentenceProcessingMaps = SentenceProcessingMaps
  { _processImplicitVerbMap = processImplicitVerbMaps
  }

player :: Player
player = Player
  { _location = bedroomInBedGID
  , _sentenceManagement = PlayerSentenceProcessingMaps playerProcessImplicitVerbMap
  , _perceptables = Perceptables mempty
  }

sentenceManagement :: SentenceProcessingMaps
sentenceManagement = SentenceProcessingMaps
  { _processImplicitVerbMap = processImplicitVerbMaps }
