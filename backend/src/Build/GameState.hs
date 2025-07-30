module Build.GameState where
import           Build.Identifiers.Actions                               (directionalStimulusActionMap,
                                                                          implicitStimulusActionMap,
                                                                          isaEnabledLookGID)
import           Build.Identifiers.Locations                             (bedroomInBedGID)
-- import           Build.Identifiers.SentenceProcessing (playerProcessImplicitVerbMap,
--                                                       processImplicitVerbMaps)
import           Build.World                                             (world)
import           Data.Map.Strict                                         (Map,
                                                                          fromList)
import           Data.Text                                               (Text)
import           Evaluators.Player.General                               (eval)
import qualified Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import qualified Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb    (look)
import           Model.GameState                                         (ActionMaps (ActionMaps),
                                                                          Config (Config, _actionMaps),
                                                                          DirectionalStimulusActionF,
                                                                          GameState (GameState, _evaluation, _narration, _player, _world),
                                                                          ImplicitStimulusActionF,
                                                                          Narration (..),
                                                                          Perceptables (Perceptables),
                                                                          Player (Player, _location, _perceptables, _playerActions),
                                                                          PlayerActions (PlayerActions),
                                                                          PlayerSentenceProcessingMaps (PlayerSentenceProcessingMaps),
                                                                          SentenceProcessingMaps (SentenceProcessingMaps, _processImplicitVerbMap))
import           Model.GID                                               (GID)
import           Model.Parser.Atomics.Verbs                              (DirectionalStimulusVerb,
                                                                          ImplicitStimulusVerb)

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
  { _actionMaps = actionMaps
  }
  where
    actionMaps :: ActionMaps
    actionMaps = ActionMaps implicitStimulusActionMap directionalStimulusActionMap

player :: Player
player = Player
  { _location = bedroomInBedGID
  , _playerActions = PlayerActions isaMap dsaMap
  , _perceptables = Perceptables mempty
  }
  where
    dsaMap :: Map DirectionalStimulusVerb (GID DirectionalStimulusActionF)
    dsaMap = mempty
    dsaLook = Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb.look
    isaMap :: Map ImplicitStimulusVerb (GID ImplicitStimulusActionF)
    isaMap = Data.Map.Strict.fromList [(isaLook, isaEnabledLookGID)]
    isaLook = Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb.look
