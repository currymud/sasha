module Build.GameState where
import           Build.BedPuzzle.Player (player)
import           Build.World            (world)
import           Data.Text              (Text)
import           Evaluators.General     (eval)
import           Model.GameState        (GameState (GameState, _evaluation, _narration, _player, _world),
                                         Narration (..))

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

