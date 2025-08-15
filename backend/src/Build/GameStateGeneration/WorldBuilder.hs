module Build.GameStateGeneration.WorldBuilder where
import           Build.GameStateGeneration.WorldGeneration               (ObjectBehaviorSpec (ObjectBehaviorSpec, _behaviors, _objectGID),
                                                                          PlayerSpec (PlayerSpec, _playerBehaviors, _playerLocationGID),
                                                                          WorldSpec (WorldSpec, _locationSpecs, _objectSpecs, _playerSpec, _spatialRelationships))
import           Build.Identifiers.Actions                               (agentCanSeeGID,
                                                                          alreadyHaveRobeFGID,
                                                                          dizzyGetFGID,
                                                                          openEyesGID,
                                                                          playerGetFGID,
                                                                          seeChairFGID,
                                                                          seeMailGID,
                                                                          seePocketRobeWornGID,
                                                                          seeRobeChairGID,
                                                                          seeRobeWornGID,
                                                                          seeTableGID,
                                                                          standDeniedGID,
                                                                          standUpGID,
                                                                          takePillFGID,
                                                                          whatPillGID)
import           Build.Identifiers.Locations                             (bedroomInBedGID,
                                                                          locationMap)
import           Build.Identifiers.Objects                               (chairObjGID,
                                                                          mailObjGID,
                                                                          objectMap,
                                                                          pillObjGID,
                                                                          pocketObjGID,
                                                                          robeObjGID,
                                                                          tableObjGID)
import qualified Data.Bifunctor
import qualified Data.Map.Strict
import           Data.Set                                                (Set)
import qualified Data.Set
import           Data.Text                                               (Text)
import           Evaluators.Player.General                               (eval)
import           Grammar.Parser.Partitions.Nouns.Objectives              (mail,
                                                                          pill,
                                                                          robe)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs        (get)
import qualified Grammar.Parser.Partitions.Verbs.ConsumptionVerbs
import qualified Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb
import qualified Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb
import           Grammar.Parser.Partitions.Verbs.PosturalVerbs           (stand)
import           Model.GameState                                         (ActionEffectKey (LocationKey, ObjectKey, PlayerKey),
                                                                          ActionEffectMap (ActionEffectMap),
                                                                          ActionKey (AcquisitionalActionKey, ConsumptionActionKey, PosturalActionKey, SomaticAccessActionKey),
                                                                          ActionManagementFunctions (ActionManagementFunctions),
                                                                          Effect (AcquisitionEffect, ConsumptionEffect, DirectionalStimulusEffect, ImplicitStimulusEffect, PerceptionEffect, PositivePosturalEffect),
                                                                          EffectRegistry,
                                                                          GameState (GameState, _effectRegistry, _evaluation, _narration, _player, _world),
                                                                          Narration (Narration, _actionConsequence, _playerAction),
                                                                          Object (_objectActionManagement),
                                                                          Player (Player, _location, _playerActions),
                                                                          PlayerKey (PlayerKeyObject),
                                                                          SpatialRelationship,
                                                                          SpatialRelationshipMap (SpatialRelationshipMap),
                                                                          World (World, _locationMap, _objectMap, _perceptionMap, _spatialRelationshipMap))
import           Model.GID                                               (GID)
import           Model.Mappings                                          (GIDToDataMap (GIDToDataMap))
import qualified Model.Parser.Atomics.Nouns
import qualified Model.Parser.Atomics.Nouns                              as Grammar.Parser.Partitions.Nouns
import           Model.Parser.Atomics.Verbs                              (ConsumptionVerb,
                                                                          DirectionalStimulusVerb,
                                                                          ImplicitStimulusVerb)
import           Model.Parser.Composites.Nouns                           (NounPhrase (SimpleNounPhrase),
                                                                          ObjectPhrase (ObjectPhrase))
import           Model.Parser.Composites.Verbs                           (AcquisitionVerbPhrase (SimpleAcquisitionVerbPhrase))
import           Relude.Foldable                                         (find)

-- Generic function that TH will call
buildWorldFromSpec :: WorldSpec -> World
buildWorldFromSpec spec = World
  { _objectMap = buildObjectMap (_objectSpecs spec)
  , _locationMap = GIDToDataMap locationMap
  , _perceptionMap = mempty
  , _spatialRelationshipMap = buildSpatialMap (_spatialRelationships spec)
  }

buildPlayerFromSpec :: PlayerSpec -> Player
buildPlayerFromSpec spec = Player
  { _location = _playerLocationGID spec
  , _playerActions = ActionManagementFunctions $ Data.Set.fromList (_playerBehaviors spec)
  }

buildObjectMap :: [ObjectBehaviorSpec] -> GIDToDataMap Object Object
buildObjectMap specs =
  let baseObjects = Data.Map.Strict.toList objectMap
      objectsWithBehaviors = map (applyBehaviorSpec specs) baseObjects
  in GIDToDataMap $ Data.Map.Strict.fromList objectsWithBehaviors

applyBehaviorSpec :: [ObjectBehaviorSpec] -> (GID Object, Object) -> (GID Object, Object)
applyBehaviorSpec specs (gid, obj) =
  case find (\spec -> _objectGID spec == gid) specs of
    Just spec -> (gid, obj { _objectActionManagement = ActionManagementFunctions $ Data.Set.fromList (_behaviors spec) })
    Nothing -> (gid, obj)


buildGameStateFromSpec :: WorldSpec -> GameState
buildGameStateFromSpec spec = GameState
  { _world = buildWorldFromSpec spec
  , _player = buildPlayerFromSpec (_playerSpec spec)
  , _narration = initNarration
  , _evaluation = eval
  , _effectRegistry = effectRegistry
  }
initNarration :: Narration
initNarration = Narration
  { _playerAction  = [initialAction]
  , _actionConsequence = mempty
  }

initialAction :: Text
initialAction = "It was a rough night. You smoked your mind the night before, on cigarettes and songs that you'd been pickin'."

buildSpatialMap :: [(GID Object, [SpatialRelationship])] -> SpatialRelationshipMap
buildSpatialMap spatialSpecs = SpatialRelationshipMap $ Data.Map.Strict.fromList $
  map (Data.Bifunctor.second Data.Set.fromList) spatialSpecs

pillObjective :: Model.Parser.Atomics.Nouns.Objective
pillObjective = pill

simpleMailOP :: ObjectPhrase
simpleMailOP = (ObjectPhrase . SimpleNounPhrase) mailObjective

mailObjective :: Model.Parser.Atomics.Nouns.Objective
mailObjective = Grammar.Parser.Partitions.Nouns.Objectives.mail

simplePillOP :: ObjectPhrase
simplePillOP = (ObjectPhrase . SimpleNounPhrase) pillObjective

standKey :: ActionKey
standKey = PosturalActionKey standDeniedGID

dizzyGetKey :: ActionKey
dizzyGetKey = AcquisitionalActionKey dizzyGetFGID


standUpKey :: ActionKey
standUpKey = PosturalActionKey standUpGID

takePillKey :: ActionKey
takePillKey = ConsumptionActionKey takePillFGID


takePillEffectMap :: ActionEffectMap
takePillEffectMap = ActionEffectMap
  $ Data.Map.Strict.fromList
      [ (PlayerKey (PlayerKeyObject pillObjGID), Data.Set.singleton pillCuresHeadacheEffect)
      ]

-- enableMailGetLocationEffect :: Effect
-- enableMailGetLocationEffect = AcquisitionEffect getMailAVP getMailGID

standUpEffectMap :: ActionEffectMap
standUpEffectMap = ActionEffectMap
  $ Data.Map.Strict.fromList
      [--  (PlayerKey (PlayerKeyObject mailObjGID), Data.Set.singleton enableMailGetEffect)
--      , (ObjectKey mailObjGID, Data.Set.singleton enableMailGetEffect)
--      , (LocationKey bedroomInBedGID, Data.Set.singleton enableMailGetLocationEffect)
      ]

-- enableMailGetEffect :: Effect
-- enableMailGetEffect = AcquisitionEffect (SimpleAcquisitionVerbPhrase get simpleMailOP) getMailGID

alreadyHaveRobeKey :: ActionKey
alreadyHaveRobeKey = AcquisitionalActionKey alreadyHaveRobeFGID

emptyEffectMap :: ActionEffectMap
emptyEffectMap = ActionEffectMap mempty

getKeyMap :: ActionEffectMap
getKeyMap = ActionEffectMap
  $ Data.Map.Strict.fromList
      [ (ObjectKey robeObjGID, Data.Set.empty)
      ]

getKey :: ActionKey
getKey = AcquisitionalActionKey playerGetFGID

effectRegistry :: EffectRegistry
effectRegistry = Data.Map.Strict.fromList
  [ (openEyesKey, openEyesEffectMap)
  , (getKey, playerGetEffectMap)
  , (alreadyHaveRobeKey, emptyEffectMap)
  , (standKey, emptyEffectMap)
  , (takePillKey, takePillEffectMap)
  , (standUpKey, standUpEffectMap)
  , (dizzyGetKey, emptyEffectMap)
  ]

openEyesKey :: ActionKey
openEyesKey  = SomaticAccessActionKey openEyesGID

openEyesEffectMap :: ActionEffectMap
openEyesEffectMap = ActionEffectMap
  $ Data.Map.Strict.fromList
      [ (bedroomOpenEyesKey, openEyesEffect)
      , (ObjectKey pillObjGID, Data.Set.singleton pillEffect)
      , (ObjectKey tableObjGID, Data.Set.singleton tableEffect)
      , (ObjectKey chairObjGID, Data.Set.singleton chairEffect)
      , (ObjectKey robeObjGID, Data.Set.singleton robeEffect)
      , (ObjectKey mailObjGID, Data.Set.singleton mailEffect)
      , ((PlayerKey (PlayerKeyObject robeObjGID)), Data.Set.singleton enableRobeGetEffect)
      ]

playerGetEffectMap :: ActionEffectMap
playerGetEffectMap = ActionEffectMap
  $ Data.Map.Strict.fromList
      [ -- Robe acquisition effects (player zone responsibility)
        (PlayerKey (PlayerKeyObject robeObjGID), Data.Set.singleton getRobeEffect)
      , (ObjectKey robeObjGID, Data.Set.singleton robeWornEffect)
      , (ObjectKey pocketObjGID, Data.Set.singleton pocketWornEffect)
      , (PlayerKey (PlayerKeyObject pillObjGID), Data.Set.fromList [pillReachableEffect, pillTakeableEffect])
      ]
pillReachableEffect :: Effect
pillReachableEffect = ConsumptionEffect takeCV pillObjGID takePillFGID

pillTakeableEffect :: Effect
pillTakeableEffect = ConsumptionEffect takeCV pillObjGID takePillFGID

pillCuresHeadacheEffect :: Effect
pillCuresHeadacheEffect = PositivePosturalEffect stand standUpGID  -- Changes stand action to successful version

takeCV :: ConsumptionVerb
takeCV = Grammar.Parser.Partitions.Verbs.ConsumptionVerbs.take

pocketWornEffect :: Effect
pocketWornEffect = DirectionalStimulusEffect dirLook seePocketRobeWornGID

openEyesEffect :: Set Effect
openEyesEffect = Data.Set.fromList [ImplicitStimulusEffect impLook agentCanSeeGID, PerceptionEffect]

impLook :: ImplicitStimulusVerb
impLook = Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb.look

dirLook :: DirectionalStimulusVerb
dirLook = Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb.look

bedroomOpenEyesKey :: ActionEffectKey
bedroomOpenEyesKey = LocationKey bedroomInBedGID

enableRobeGetEffect :: Effect
enableRobeGetEffect = AcquisitionEffect (SimpleAcquisitionVerbPhrase get simpleRobeOP) playerGetFGID

mailEffect :: Effect
mailEffect = DirectionalStimulusEffect dirLook seeMailGID

getRobeAVP :: AcquisitionVerbPhrase
getRobeAVP = SimpleAcquisitionVerbPhrase get simpleRobeOP

getRobeEffect :: Effect
getRobeEffect = AcquisitionEffect getRobeAVP alreadyHaveRobeFGID

getRobeEffectMap :: ActionEffectMap
getRobeEffectMap = ActionEffectMap
  $ Data.Map.Strict.fromList
      [ (PlayerKey (PlayerKeyObject robeObjGID), Data.Set.singleton getRobeEffect)
      , (ObjectKey robeObjGID, Data.Set.singleton robeWornEffect)
      ]
robeWornEffect :: Effect
robeWornEffect = DirectionalStimulusEffect dirLook seeRobeWornGID

simpleRobeOP :: ObjectPhrase
simpleRobeOP = (ObjectPhrase . SimpleNounPhrase) robeObjective

robeObjective :: Model.Parser.Atomics.Nouns.Objective
robeObjective = robe

robeEffect :: Effect
robeEffect = DirectionalStimulusEffect dirLook seeRobeChairGID
pillEffect :: Effect
pillEffect = DirectionalStimulusEffect dirLook whatPillGID
tableEffect :: Effect
tableEffect = DirectionalStimulusEffect dirLook seeTableGID
chairEffect :: Effect
chairEffect = DirectionalStimulusEffect dirLook seeChairFGID
