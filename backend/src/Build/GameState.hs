module Build.GameState where
import           Build.Identifiers.Actions                               (agentCanSeeGID,
                                                                          directionalStimulusActionMap,
                                                                          dsvEnabledLookGID,
                                                                          implicitStimulusActionMap,
                                                                          isaEnabledLookGID,
                                                                          openEyesGID,
                                                                          seeChairGID,
                                                                          seePillGID,
                                                                          seeTableGID,
                                                                          somaticAccessActionMap,
                                                                          whatPillGID)
import           Build.Identifiers.Locations                             (bedroomInBedGID)
import           Build.Identifiers.Objects                               (chairObjGID,
                                                                          initialInventoryGID,
                                                                          pillObjGID,
                                                                          tableObjGID)
import           Build.World                                             (world)
import           Data.Map.Strict                                         (Map,
                                                                          empty,
                                                                          fromList)
import qualified Data.Set
import           Data.Text                                               (Text,
                                                                          empty)
import           Evaluators.Player.General                               (eval)
import qualified Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import qualified Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb    (look)
import qualified Grammar.Parser.Partitions.Verbs.SomaticAccessVerbs      (open)
import           Model.GameState                                         (AcquisitionActionF,
                                                                          ActionEffectKey (LocationKey, ObjectKey),
                                                                          ActionEffectMap (ActionEffectMap),
                                                                          ActionKey (SomaticAccessActionKey),
                                                                          ActionKeyMap (ActionKeyMap),
                                                                          ActionMaps (ActionMaps),
                                                                          Config (Config, _actionMaps),
                                                                          DirectionalStimulusActionF,
                                                                          Effect (DirectionalStimulusEffect, ImplicitStimulusEffect),
                                                                          GameState (GameState, _evaluation, _narration, _player, _world),
                                                                          ImplicitStimulusActionF,
                                                                          Narration (..),
                                                                          Object (Object, _description, _descriptives, _objectActionManagement, _shortName),
                                                                          Perceptables (Perceptables),
                                                                          Player (Player, _actionKeyMap, _inventory, _location, _perceptables, _playerActions),
                                                                          PlayerActions (PlayerActions),
                                                                          SomaticAccessActionF)
import           Model.GID                                               (GID)
import           Model.Parser.Atomics.Verbs                              (AcquisitionVerb,
                                                                          DirectionalStimulusVerb,
                                                                          ImplicitStimulusVerb,
                                                                          SomaticAccessVerb)
import           Model.Parser.Composites.Verbs                           (AcquisitionVerbPhrase)

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
    actionMaps = ActionMaps implicitStimulusActionMap directionalStimulusActionMap somaticAccessActionMap acquisitionVerbMap
    acquisitionVerbMap :: Map (GID AcquisitionActionF) AcquisitionActionF
    acquisitionVerbMap = Data.Map.Strict.empty

player :: Player
player = Player
  { _location = bedroomInBedGID
  , _playerActions = PlayerActions isaMap dsaMap saMap acquisitionVerbs
  , _perceptables = Perceptables mempty
  , _actionKeyMap = actionKeyMap
  , _inventory = initialInventoryGID
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
    acquisitionVerbs :: Map AcquisitionVerbPhrase (GID AcquisitionActionF)
    acquisitionVerbs = Data.Map.Strict.empty

actionKeyMap :: ActionKeyMap
actionKeyMap = ActionKeyMap
  $ fromList
      [ (openEyesKey,openEyesEffectMap)
      ]

openEyesKey :: ActionKey
openEyesKey  = SomaticAccessActionKey openEyesGID

openEyesEffectMap :: ActionEffectMap
openEyesEffectMap = ActionEffectMap
  $ fromList
      [ (bedroomOpenEyesKey, Data.Set.singleton openEyesEffect)
      , (ObjectKey pillObjGID, Data.Set.singleton pillEffect)
      , (ObjectKey tableObjGID, Data.Set.singleton tableEffect)
      , (ObjectKey chairObjGID, Data.Set.singleton chairEffect)
      ]

bedroomOpenEyesKey :: ActionEffectKey
bedroomOpenEyesKey = LocationKey bedroomInBedGID


openEyesEffect :: Effect
openEyesEffect = ImplicitStimulusEffect impLook agentCanSeeGID

impLook :: ImplicitStimulusVerb
impLook = Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb.look

dirLook :: DirectionalStimulusVerb
dirLook = Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb.look

pillEffect :: Effect
pillEffect = DirectionalStimulusEffect dirLook whatPillGID
tableEffect :: Effect
tableEffect = DirectionalStimulusEffect dirLook seeTableGID
chairEffect :: Effect
chairEffect = DirectionalStimulusEffect dirLook seeChairGID
