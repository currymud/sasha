module Build.GameState where
import           Build.Identifiers.Actions                               (acquisitionActionMap,
                                                                          agentCanSeeGID,
                                                                          alreadyHaveRobeGID,
                                                                          checkInventoryGID,
                                                                          directionalStimulusActionMap,
                                                                          dsvEnabledLookGID,
                                                                          getPillDeniedGID,
                                                                          getRobeDeniedGID,
                                                                          getRobeGID,
                                                                          implicitStimulusActionMap,
                                                                          isaEnabledLookGID,
                                                                          openEyesGID,
                                                                          playerGetGID,
                                                                          seeChairGID,
                                                                          seeRobeChairGID,
                                                                          seeTableGID,
                                                                          somaticAccessActionMap,
                                                                          whatPillGID)
import           Build.Identifiers.Locations                             (bedroomInBedGID)
import           Build.Identifiers.Objects                               (chairObjGID,
                                                                          pillObjGID,
                                                                          robeObjGID,
                                                                          tableObjGID)
import           Build.World                                             (world)
import           Data.Map.Strict                                         (Map,
                                                                          empty,
                                                                          fromList)
import qualified Data.Set
import           Data.Text                                               (Text)
import           Evaluators.Player.General                               (eval)
import           Grammar.Parser.Partitions.Nouns.Objectives              (pill,
                                                                          robe)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs        (get)
import qualified Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb    (inventory)
import qualified Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb    (look)
import qualified Grammar.Parser.Partitions.Verbs.SomaticAccessVerbs      (open)
import           Model.GameState                                         (AcquisitionActionF,
                                                                          ActionEffectKey (LocationKey, ObjectKey, PlayerKey),
                                                                          ActionEffectMap (ActionEffectMap),
                                                                          ActionKey (AcquisitionalActionKey, SomaticAccessActionKey),
                                                                          ActionKeyMap (ActionKeyMap),
                                                                          ActionMaps (ActionMaps),
                                                                          Config (Config, _actionMaps),
                                                                          DirectionalStimulusActionF,
                                                                          Effect (AcquisitionEffect, DirectionalStimulusEffect, ImplicitStimulusEffect),
                                                                          GameState (GameState, _evaluation, _narration, _player, _world),
                                                                          ImplicitStimulusActionF,
                                                                          Narration (..),
                                                                          Perceptables (Perceptables),
                                                                          Player (Player, _actionKeyMap, _inventory, _location, _perceptables, _playerActions),
                                                                          PlayerActions (PlayerActions),
                                                                          PlayerKey (PlayerKeyObject),
                                                                          SomaticAccessActionF)
import           Model.GID                                               (GID)
import qualified Model.Parser.Atomics.Nouns
import           Model.Parser.Atomics.Verbs                              (DirectionalStimulusVerb,
                                                                          ImplicitStimulusVerb,
                                                                          SomaticAccessVerb)
import           Model.Parser.Composites.Nouns                           (NounPhrase (SimpleNounPhrase),
                                                                          ObjectPhrase (ObjectPhrase))
import           Model.Parser.Composites.Verbs                           (AcquisitionVerbPhrase (SimpleAcquisitionVerbPhrase))

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
    actionMaps = ActionMaps
                   implicitStimulusActionMap
                   directionalStimulusActionMap
                   somaticAccessActionMap
                   acquisitionActionMap

player :: Player
player = Player
  { _location = bedroomInBedGID
  , _playerActions = PlayerActions isaMap dsaMap saMap acquisitionVerbs
  , _perceptables = Perceptables mempty
  , _actionKeyMap = actionKeyMap
  , _inventory = Data.Map.Strict.empty
  }
  where
    dsaMap :: Map DirectionalStimulusVerb (GID DirectionalStimulusActionF)
    dsaMap = Data.Map.Strict.fromList [(dsaLook, dsvEnabledLookGID)]
    dsaLook = Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb.look
    isaMap :: Map ImplicitStimulusVerb (GID ImplicitStimulusActionF)
    isaMap = Data.Map.Strict.fromList [(isaLook, isaEnabledLookGID)
                                      ,(inventory,checkInventoryGID)]
    isaLook = Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb.look
    saOpen = Grammar.Parser.Partitions.Verbs.SomaticAccessVerbs.open
    saMap :: Map SomaticAccessVerb (GID SomaticAccessActionF)
    saMap = Data.Map.Strict.fromList [(saOpen, openEyesGID)]
    acquisitionVerbs :: Map AcquisitionVerbPhrase (GID AcquisitionActionF)
    acquisitionVerbs = Data.Map.Strict.fromList [(SimpleAcquisitionVerbPhrase get simplePillOP, getPillDeniedGID)
                                                , (SimpleAcquisitionVerbPhrase get simpleRobeOP, getRobeDeniedGID)]

pillObjective :: Model.Parser.Atomics.Nouns.Objective
pillObjective = pill

simplePillOP :: ObjectPhrase
simplePillOP = (ObjectPhrase . SimpleNounPhrase) pillObjective

robeObjective :: Model.Parser.Atomics.Nouns.Objective
robeObjective = robe

simpleRobeOP :: ObjectPhrase
simpleRobeOP = (ObjectPhrase . SimpleNounPhrase) robeObjective

-- Missing action keys for pill
getPillDeniedKey :: ActionKey
getPillDeniedKey = AcquisitionalActionKey getPillDeniedGID

getPillDeniedEffectMap :: ActionEffectMap
getPillDeniedEffectMap = ActionEffectMap Data.Map.Strict.empty

-- Missing action keys for robe (initially denied)
getRobeDeniedKey :: ActionKey
getRobeDeniedKey = AcquisitionalActionKey getRobeDeniedGID

getRobeDeniedEffectMap :: ActionEffectMap
getRobeDeniedEffectMap = ActionEffectMap Data.Map.Strict.empty

actionKeyMap :: ActionKeyMap
actionKeyMap = ActionKeyMap
  $ fromList
      [ (openEyesKey,openEyesEffectMap)
      , (getKey, getKeyMap)
      , (getRobeKey, getRobeEffectMap)
      , (alreadyHaveRobeKey, alreadyHaveRobeEffectMap)
      , (getPillDeniedKey, getPillDeniedEffectMap)  -- Add this for pill
      , (getRobeDeniedKey, getRobeDeniedEffectMap)
      ]

alreadyHaveRobeKey :: ActionKey
alreadyHaveRobeKey = AcquisitionalActionKey alreadyHaveRobeGID

alreadyHaveRobeEffectMap :: ActionEffectMap
alreadyHaveRobeEffectMap = ActionEffectMap Data.Map.Strict.empty

getKeyMap :: ActionEffectMap
getKeyMap = ActionEffectMap
  $ fromList
      [ (ObjectKey robeObjGID, Data.Set.singleton getRobeEffect)
      ]

getKey :: ActionKey
getKey = AcquisitionalActionKey playerGetGID

openEyesKey :: ActionKey
openEyesKey  = SomaticAccessActionKey openEyesGID

openEyesEffectMap :: ActionEffectMap
openEyesEffectMap = ActionEffectMap
  $ fromList
      [ (bedroomOpenEyesKey, Data.Set.singleton openEyesEffect)
      , (ObjectKey pillObjGID, Data.Set.singleton pillEffect)
      , (ObjectKey tableObjGID, Data.Set.singleton tableEffect)
      , (ObjectKey chairObjGID, Data.Set.singleton chairEffect)
      , (ObjectKey robeObjGID, Data.Set.singleton robeEffect)
      ]

bedroomOpenEyesKey :: ActionEffectKey
bedroomOpenEyesKey = LocationKey bedroomInBedGID


openEyesEffect :: Effect
openEyesEffect = ImplicitStimulusEffect impLook agentCanSeeGID

impLook :: ImplicitStimulusVerb
impLook = Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb.look

dirLook :: DirectionalStimulusVerb
dirLook = Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb.look

getRobeEffect :: Effect
getRobeEffect = AcquisitionEffect get alreadyHaveRobeGID

getRobeKey :: ActionKey
getRobeKey = AcquisitionalActionKey getRobeGID

getRobeEffectMap :: ActionEffectMap
getRobeEffectMap = ActionEffectMap
  $ fromList
      [ (PlayerKey (PlayerKeyObject robeObjGID), Data.Set.singleton getRobeEffect) ]

robeEffect :: Effect
robeEffect = DirectionalStimulusEffect dirLook seeRobeChairGID
pillEffect :: Effect
pillEffect = DirectionalStimulusEffect dirLook whatPillGID
tableEffect :: Effect
tableEffect = DirectionalStimulusEffect dirLook seeTableGID
chairEffect :: Effect
chairEffect = DirectionalStimulusEffect dirLook seeChairGID
