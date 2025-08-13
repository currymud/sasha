{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Build.GameState where
import           Build.BedPuzzle.Actions.Objects.Mail                    (getMailAVP)
import           Build.BedPuzzle.Actions.Objects.Pill                    (takePillCVP)
import           Build.Identifiers.Actions                               (acquisitionActionMap,
                                                                          agentCanSeeGID,
                                                                          alreadyHaveRobeGID,
                                                                          checkInventoryGID,
                                                                          consumptionActionMap,
                                                                          directionalStimulusActionMap,
                                                                          dizzyGetGID,
                                                                          dsvEnabledLookGID,
                                                                          getMailDeniedGID,
                                                                          getMailGID,
                                                                          getRobeGID,
                                                                          implicitStimulusActionMap,
                                                                          isaEnabledLookGID,
                                                                          openEyesGID,
                                                                          pillTooFarFGID,
                                                                          playerGetGID,
                                                                          posturalActionMap,
                                                                          seeChairGID,
                                                                          seeMailGID,
                                                                          seePocketRobeWornGID,
                                                                          seeRobeChairGID,
                                                                          seeRobeWornGID,
                                                                          seeTableGID,
                                                                          somaticAccessActionMap,
                                                                          standDeniedGID,
                                                                          standUpGID,
                                                                          takePillFGID,
                                                                          whatPillGID)
import           Build.Identifiers.Locations                             (bedroomInBedGID)
import           Build.Identifiers.Objects                               (chairObjGID,
                                                                          mailObjGID,
                                                                          pillObjGID,
                                                                          pocketObjGID,
                                                                          robeObjGID,
                                                                          tableObjGID)
import           Build.World                                             (world)
import           Data.Map.Strict                                         (empty,
                                                                          fromList)
import qualified Data.Map.Strict                                         as Data.MapStrict
import           Data.Set                                                (Set)
import qualified Data.Set
import           Data.Text                                               (Text)
import           Evaluators.Player.General                               (eval)
import           Grammar.Parser.Partitions.Nouns.Objectives              (mail,
                                                                          pill,
                                                                          robe)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs        (get)
import           Grammar.Parser.Partitions.Verbs.ConsumptionVerbs        (take)
import qualified Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb    (inventory)
import qualified Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb    (look)
import           Grammar.Parser.Partitions.Verbs.PosturalVerbs           (stand)
import qualified Grammar.Parser.Partitions.Verbs.SomaticAccessVerbs      (open)
import           Model.GameState                                         (ActionEffectKey (LocationKey, ObjectKey, PlayerKey),
                                                                          ActionEffectMap (ActionEffectMap),
                                                                          ActionKey (AcquisitionalActionKey, ConsumptionActionKey, PosturalActionKey, SomaticAccessActionKey),
                                                                          ActionKeyMap (ActionKeyMap),
                                                                          ActionManagement (AAManagementKey, CAManagementKey, DSAManagementKey, ISAManagementKey, PPManagementKey, SSAManagementKey),
                                                                          ActionManagementFunctions (ActionManagementFunctions),
                                                                          ActionMaps (ActionMaps),
                                                                          Config (Config, _actionMaps),
                                                                          Effect (AcquisitionEffect, ConsumptionEffect, DirectionalStimulusEffect, ImplicitStimulusEffect, PerceptionEffect, PositivePosturalEffect),
                                                                          GameState (GameState, _evaluation, _narration, _player, _world),
                                                                          Narration (..),
                                                                          Player (Player, _location, _playerActions),
                                                                          PlayerKey (PlayerKeyObject))
import           Model.Parser.Atomics.Adverbs                            (PositivePosturalDirection (PositivePosturalDirection))
import qualified Model.Parser.Atomics.Nouns
import           Model.Parser.Atomics.Verbs                              (ConsumptionVerb,
                                                                          DirectionalStimulusVerb,
                                                                          ImplicitStimulusVerb)
import           Model.Parser.Composites.Nouns                           (NounPhrase (SimpleNounPhrase),
                                                                          ObjectPhrase (ObjectPhrase))
import           Model.Parser.Composites.Verbs                           (AcquisitionVerbPhrase (SimpleAcquisitionVerbPhrase),
                                                                          ConsumptionVerbPhrase)
import           Prelude                                                 hiding
                                                                         (take)
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
                   consumptionActionMap
                   posturalActionMap
player :: Player
player = Player
  { _location = bedroomInBedGID
  , _playerActions = playerActionMgmt
  }
  where
    dsaLook = Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb.look
    isaLook = Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb.look
    saOpen = Grammar.Parser.Partitions.Verbs.SomaticAccessVerbs.open
    playerActionMgmt :: ActionManagementFunctions
    playerActionMgmt = ActionManagementFunctions $ Data.Set.fromList
      [ ISAManagementKey isaLook isaEnabledLookGID
      , ISAManagementKey inventory checkInventoryGID
      , CAManagementKey takePillCVP pillTooFarFGID
      , DSAManagementKey dsaLook dsvEnabledLookGID
      , SSAManagementKey saOpen openEyesGID
      , AAManagementKey (SimpleAcquisitionVerbPhrase get simplePillOP) playerGetGID
      , AAManagementKey (SimpleAcquisitionVerbPhrase get simpleRobeOP) dizzyGetGID
      , AAManagementKey (SimpleAcquisitionVerbPhrase get simpleMailOP) playerGetGID
      , PPManagementKey stand standDeniedGID
      ]

pillObjective :: Model.Parser.Atomics.Nouns.Objective
pillObjective = pill

simpleMailOP :: ObjectPhrase
simpleMailOP = (ObjectPhrase . SimpleNounPhrase) mailObjective

mailObjective :: Model.Parser.Atomics.Nouns.Objective
mailObjective = Grammar.Parser.Partitions.Nouns.Objectives.mail

simplePillOP :: ObjectPhrase
simplePillOP = (ObjectPhrase . SimpleNounPhrase) pillObjective

robeObjective :: Model.Parser.Atomics.Nouns.Objective
robeObjective = robe

simpleRobeOP :: ObjectPhrase
simpleRobeOP = (ObjectPhrase . SimpleNounPhrase) robeObjective

standKey :: ActionKey
standKey = PosturalActionKey standDeniedGID

actionKeyMap :: ActionKeyMap
actionKeyMap = ActionKeyMap
  $ fromList
      [ (openEyesKey,openEyesEffectMap)
      , (getKey, playerGetEffectMap)
      , (alreadyHaveRobeKey, emptyEffectMap)
      , (standKey, emptyEffectMap)
      , (takePillKey, takePillEffectMap)
      , (standUpKey, standUpEffectMap)
      , (dizzyGetKey, emptyEffectMap)
      ]

dizzyGetKey :: ActionKey
dizzyGetKey = AcquisitionalActionKey dizzyGetGID


standUpKey :: ActionKey
standUpKey = PosturalActionKey standUpGID

takePillKey :: ActionKey
takePillKey = ConsumptionActionKey takePillFGID


takePillEffectMap :: ActionEffectMap
takePillEffectMap = ActionEffectMap
  $ fromList
      [ (PlayerKey (PlayerKeyObject pillObjGID), Data.Set.singleton pillCuresHeadacheEffect)
      ]

enableMailGetLocationEffect :: Effect
enableMailGetLocationEffect = AcquisitionEffect getMailAVP getMailGID

standUpEffectMap :: ActionEffectMap
standUpEffectMap = ActionEffectMap
  $ fromList
      [ (PlayerKey (PlayerKeyObject mailObjGID), Data.Set.singleton enableMailGetEffect)
      , (ObjectKey mailObjGID, Data.Set.singleton enableMailGetEffect)
      , (LocationKey bedroomInBedGID, Data.Set.singleton enableMailGetLocationEffect)
      ]

enableMailGetEffect :: Effect
enableMailGetEffect = AcquisitionEffect (SimpleAcquisitionVerbPhrase get simpleMailOP) getMailGID

alreadyHaveRobeKey :: ActionKey
alreadyHaveRobeKey = AcquisitionalActionKey alreadyHaveRobeGID

emptyEffectMap :: ActionEffectMap
emptyEffectMap = ActionEffectMap mempty

getKeyMap :: ActionEffectMap
getKeyMap = ActionEffectMap
  $ fromList
      [ (ObjectKey robeObjGID, Data.Set.empty)
      ]

getKey :: ActionKey
getKey = AcquisitionalActionKey playerGetGID

openEyesKey :: ActionKey
openEyesKey  = SomaticAccessActionKey openEyesGID

openEyesEffectMap :: ActionEffectMap
openEyesEffectMap = ActionEffectMap
  $ fromList
      [ (bedroomOpenEyesKey, openEyesEffect)
      , (ObjectKey pillObjGID, Data.Set.singleton pillEffect)
      , (ObjectKey tableObjGID, Data.Set.singleton tableEffect)
      , (ObjectKey chairObjGID, Data.Set.singleton chairEffect)
      , (ObjectKey robeObjGID, Data.Set.singleton robeEffect)
      , (ObjectKey mailObjGID, Data.Set.singleton mailEffect)
      , (PlayerKey (PlayerKeyObject robeObjGID), Data.Set.singleton enableRobeGetEffect)
      ]

enableRobeGetEffect :: Effect
enableRobeGetEffect = AcquisitionEffect (SimpleAcquisitionVerbPhrase get simpleRobeOP) playerGetGID

mailEffect :: Effect
mailEffect = DirectionalStimulusEffect dirLook seeMailGID

playerGetEffectMap :: ActionEffectMap
playerGetEffectMap = ActionEffectMap
  $ fromList
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
takeCV = take

bedroomOpenEyesKey :: ActionEffectKey
bedroomOpenEyesKey = LocationKey bedroomInBedGID

pocketWornEffect :: Effect
pocketWornEffect = DirectionalStimulusEffect dirLook seePocketRobeWornGID

openEyesEffect :: Set Effect
openEyesEffect = Data.Set.fromList [ImplicitStimulusEffect impLook agentCanSeeGID, PerceptionEffect]

impLook :: ImplicitStimulusVerb
impLook = Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb.look

dirLook :: DirectionalStimulusVerb
dirLook = Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb.look

getRobeAVP :: AcquisitionVerbPhrase
getRobeAVP = SimpleAcquisitionVerbPhrase get simpleRobeOP

getRobeEffect :: Effect
getRobeEffect = AcquisitionEffect getRobeAVP alreadyHaveRobeGID

getRobeKey :: ActionKey
getRobeKey = AcquisitionalActionKey getRobeGID

getRobeEffectMap :: ActionEffectMap
getRobeEffectMap = ActionEffectMap
  $ fromList
      [ (PlayerKey (PlayerKeyObject robeObjGID), Data.Set.singleton getRobeEffect)
      , (ObjectKey robeObjGID, Data.Set.singleton robeWornEffect)
      ]

robeWornEffect :: Effect
robeWornEffect = DirectionalStimulusEffect dirLook seeRobeWornGID

robeEffect :: Effect
robeEffect = DirectionalStimulusEffect dirLook seeRobeChairGID
pillEffect :: Effect
pillEffect = DirectionalStimulusEffect dirLook whatPillGID
tableEffect :: Effect
tableEffect = DirectionalStimulusEffect dirLook seeTableGID
chairEffect :: Effect
chairEffect = DirectionalStimulusEffect dirLook seeChairGID
