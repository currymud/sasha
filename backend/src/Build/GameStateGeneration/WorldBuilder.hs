{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Build.GameStateGeneration.WorldBuilder where

import           Build.BedPuzzle.Actions.Get.Constructors                (getFromSupportF,
                                                                          getObjectF)
import           Build.GameStateGeneration.LocationSpec.LocationGIDs     (bedroomGID)
import           Build.GameStateGeneration.LocationSpec.Locations        (locationMap)
import           Build.GameStateGeneration.ObjectSpec.ObjectGIDS         (chairGID,
                                                                          mailGID,
                                                                          pillGID,
                                                                          pocketGID,
                                                                          robeGID,
                                                                          smallTableGID)
import           Build.GameStateGeneration.ObjectSpec.Objects            (objectMap)
import           Build.GameStateGeneration.WorldGeneration               (ObjectBehaviorSpec (ObjectBehaviorSpec, _behaviors, _objectGID),
                                                                          PlayerSpec (PlayerSpec, _playerBehaviors, _playerLocationGID),
                                                                          WorldSpec (WorldSpec, _locationSpecs, _objectSpecs, _playerSpec, _spatialRelationships))
import           Build.Identifiers.Actions                               (agentCanSeeGID,
                                                                          alreadyHaveRobeFGID,
                                                                          dizzyGetFGID,
                                                                          getFromChairFGID,
                                                                          getFromRobeFGID,
                                                                          openEyesGID,
                                                                          playerGetFGID,
                                                                          robeCollectedFGID,
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
import qualified Grammar.Parser.Partitions.Verbs.SomaticAccessVerbs
import           Model.GameState                                         (AcquisitionActionF,
                                                                          ActionEffectKey (LocationKey, ObjectKey, PlayerKey),
                                                                          ActionEffectMap (ActionEffectMap),
                                                                          ActionKey (AcquisitionalActionKey, ConsumptionActionKey, PosturalActionKey, SomaticAccessActionKey),
                                                                          ActionManagementFunctions (ActionManagementFunctions),
                                                                          Effect (AcquisitionPhraseEffect, AcquisitionVerbEffect, ConsumptionEffect, DirectionalStimulusEffect, ImplicitStimulusEffect, PerceptionEffect, PositivePosturalEffect),
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
import           Model.Parser.Atomics.Verbs                              (AcquisitionVerb (AcquisitionVerb),
                                                                          ConsumptionVerb (ConsumptionVerb),
                                                                          DirectionalStimulusVerb,
                                                                          ImplicitStimulusVerb,
                                                                          SomaticAccessVerb)
import           Model.Parser.Composites.Nouns                           (NounPhrase (SimpleNounPhrase),
                                                                          ObjectPhrase (ObjectPhrase))
import           Model.Parser.Composites.Verbs                           (AcquisitionVerbPhrase (SimpleAcquisitionVerbPhrase))
import           Relude.Foldable                                         (find)

-- Generic function that TH will call
buildWorldFromSpec :: WorldSpec -> World
buildWorldFromSpec spec = World
  { _objectMap = objectMap
  , _locationMap = locationMap
  , _perceptionMap = mempty
  , _spatialRelationshipMap = buildSpatialMap (_spatialRelationships spec)
  }

buildPlayerFromSpec :: PlayerSpec -> Player
buildPlayerFromSpec spec = Player
  { _location = _playerLocationGID spec
  , _playerActions = ActionManagementFunctions $ Data.Set.fromList (_playerBehaviors spec)
  }

buildGameStateFromSpec :: WorldSpec -> PlayerSpec -> GameState
buildGameStateFromSpec w_spec p_spec = GameState
  { _world = buildWorldFromSpec w_spec
  , _player = buildPlayerFromSpec p_spec
  , _narration = initNarration
  , _evaluation = eval
  , _effectRegistry = effectRegistry
  }
initNarration :: Narration
initNarration = Narration
  { _playerAction  = [initialAction]
  , _actionConsequence = mempty
  }

dsaLook :: DirectionalStimulusVerb
dsaLook = Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb.look

isaLook :: ImplicitStimulusVerb
isaLook = Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb.look

saOpen :: SomaticAccessVerb
saOpen = Grammar.Parser.Partitions.Verbs.SomaticAccessVerbs.open

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
      [ (PlayerKey (PlayerKeyObject pillGID), Data.Set.singleton pillCuresHeadacheEffect)
      ]

-- enableMailGetLocationEffect :: Effect
-- enableMailGetLocationEffect = AcquisitionEffect getMailAVP getMailGID

standUpEffectMap :: ActionEffectMap
standUpEffectMap = ActionEffectMap
  $ Data.Map.Strict.fromList
      [-- (PlayerKey (PlayerKeyObject mailGID), Data.Set.singleton enableMailGetEffect)
--       (ObjectKey mailGID, Data.Set.singleton enableMailGetEffect)
--      , (LocationKey bedroomGID, Data.Set.singleton enableMailGetLocationEffect)
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
      [ (ObjectKey robeGID, Data.Set.empty)
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
      , (ObjectKey pillGID, Data.Set.singleton pillEffect)
      , (ObjectKey smallTableGID, Data.Set.singleton tableEffect)
      , (ObjectKey chairGID, Data.Set.fromList [chairLookEffect, getFromChairEffect])
      , (ObjectKey robeGID, Data.Set.fromList [robeLookEffect,robeToGetEffect, getFromRobeEffect])
      , (ObjectKey mailGID, Data.Set.singleton mailEffect)
      , (PlayerKey (PlayerKeyObject robeGID), Data.Set.singleton enableRobeGetEffect)
      ]

playerGetEffectMap :: ActionEffectMap
playerGetEffectMap = ActionEffectMap
  $ Data.Map.Strict.fromList
      [ -- Robe acquisition effects (player zone responsibility)
        (PlayerKey (PlayerKeyObject robeGID), Data.Set.singleton getRobeEffect)
      , (ObjectKey robeGID, Data.Set.singleton robeWornEffect)
      , (ObjectKey pocketGID, Data.Set.singleton pocketWornEffect)
      , (PlayerKey (PlayerKeyObject pillGID), Data.Set.fromList [pillReachableEffect, pillTakeableEffect])
      ]
pillReachableEffect :: Effect
pillReachableEffect = ConsumptionEffect takeCV pillGID takePillFGID

pillTakeableEffect :: Effect
pillTakeableEffect = ConsumptionEffect takeCV pillGID takePillFGID

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
bedroomOpenEyesKey = LocationKey bedroomGID

enableRobeGetEffect :: Effect
enableRobeGetEffect = AcquisitionPhraseEffect (SimpleAcquisitionVerbPhrase get simpleRobeOP) playerGetFGID

mailEffect :: Effect
mailEffect = DirectionalStimulusEffect dirLook seeMailGID

getRobeAVP :: AcquisitionVerbPhrase
getRobeAVP = SimpleAcquisitionVerbPhrase get simpleRobeOP

getRobeEffect :: Effect
getRobeEffect = AcquisitionPhraseEffect getRobeAVP alreadyHaveRobeFGID

robeToGetEffect :: Effect
robeToGetEffect = AcquisitionPhraseEffect getRobeAVP robeCollectedFGID
getFromRobeEffect :: Effect
getFromRobeEffect = AcquisitionVerbEffect get getFromRobeFGID

getRobeEffectMap :: ActionEffectMap
getRobeEffectMap = ActionEffectMap
  $ Data.Map.Strict.fromList
      [ (PlayerKey (PlayerKeyObject robeGID), Data.Set.singleton getRobeEffect)
      , (ObjectKey robeGID, Data.Set.singleton robeWornEffect)
      ]

getRobe :: AcquisitionActionF
getRobe = getObjectF robeGID

robeWornEffect :: Effect
robeWornEffect = DirectionalStimulusEffect dirLook seeRobeWornGID

simpleRobeOP :: ObjectPhrase
simpleRobeOP = (ObjectPhrase . SimpleNounPhrase) robeObjective

robeObjective :: Model.Parser.Atomics.Nouns.Objective
robeObjective = robe

robeLookEffect :: Effect
robeLookEffect = DirectionalStimulusEffect dirLook seeRobeChairGID


pillEffect :: Effect
pillEffect = DirectionalStimulusEffect dirLook whatPillGID
tableEffect :: Effect
tableEffect = DirectionalStimulusEffect dirLook seeTableGID

chairLookEffect :: Effect
chairLookEffect = DirectionalStimulusEffect dirLook seeChairFGID

getFromChairEffect :: Effect
getFromChairEffect = AcquisitionVerbEffect get getFromChairFGID
