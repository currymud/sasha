module Build.GameState where
import           Build.Identifiers.Actions                               (directionalStimulusActionMap,
                                                                          dsvEnabledLookGID,
                                                                          implicitStimulusActionMap,
                                                                          isaEnabledLookGID,
                                                                          openEyesGID,
                                                                          somaticAccessActionMap)
import           Build.Identifiers.Locations                             (bedroomInBedGID)
import           Build.World                                             (world)
import           Data.Map.Strict                                         (Map,
                                                                          empty,
                                                                          fromList)
import           Data.Text                                               (Text)
import           Evaluators.Player.General                               (eval)
import qualified Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import qualified Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb    (look)
import qualified Grammar.Parser.Partitions.Verbs.SomaticAccessVerbs      (open)
import           Model.GameState                                         (ActionEffect,
                                                                          ActionEffectKey,
                                                                          ActionMaps (ActionMaps),
                                                                          Config (Config, _actionMaps),
                                                                          DirectionalStimulusActionF,
                                                                          GameState (GameState, _evaluation, _narration, _player, _world),
                                                                          ImplicitStimulusActionF,
                                                                          Narration (..),
                                                                          Perceptables (Perceptables),
                                                                          Player (Player, _location, _perceptables, _playerActions),
                                                                          PlayerActions (PlayerActions),
                                                                          PlayerEffects (PlayerEffects),
                                                                          SomaticAccessActionF)
import           Model.GID                                               (GID)
import           Model.Parser.Atomics.Verbs                              (DirectionalStimulusVerb,
                                                                          ImplicitStimulusVerb,
                                                                          SomaticAccessVerb)

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
    actionMaps = ActionMaps implicitStimulusActionMap directionalStimulusActionMap somaticAccessActionMap

player :: Player
player = Player
  { _location = bedroomInBedGID
  , _playerActions = PlayerActions isaMap dsaMap saMap
  , _perceptables = Perceptables mempty
  }
  where
    dsaMap :: Map DirectionalStimulusVerb (GID DirectionalStimulusActionF)
    dsaMap = Data.Map.Strict.fromList [(dsaLook, dsvEnabledLookGID)]
    dsaLook = Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb.look
    isaMap :: Map ImplicitStimulusVerb (GID ImplicitStimulusActionF)
    isaMap = Data.Map.Strict.fromList [(isaLook, isaEnabledLookGID)]
    isaLook = Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb.look
    saOpen = Grammar.Parser.Partitions.Verbs.SomaticAccessVerbs.open
    saMap :: Map SomaticAccessVerb (GID SomaticAccessActionF)
    saMap = Data.Map.Strict.fromList [(saOpen, openEyesGID)]
