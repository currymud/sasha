module Build.GameState where
import           Build.Identifiers.Locations          (bedroomInBedGID)
import           Build.Identifiers.SentenceProcessing (playerProcessImplicitVerbMap,
                                                       processImplicitVerbMap)
import           Build.World                          (world)
import           Data.Text                            (Text)
import           Evaluators.Player.General            (eval)
import           Model.GameState                      (GameState (GameState, _evaluation, _narration, _player, _world),
                                                       Narration (..),
                                                       Player (Player, _location, _sentenceManagement),
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

player :: Player
player = Player
  { _location = bedroomInBedGID
  , _sentenceManagement = playerProcessImplicitVerbMap
  }

sentenceManagement :: SentenceProcessingMaps
sentenceManagement = SentenceProcessingMaps
  { _processImplicitVerbMap = processImplicitVerbMap }
