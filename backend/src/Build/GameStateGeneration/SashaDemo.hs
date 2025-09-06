{-# LANGUAGE OverloadedStrings #-}

module Build.GameStateGeneration.SashaDemo where

import           Build.GameStateGeneration.Defaults                      (defaultLocation,
                                                                          defaultObject,
                                                                          defaultPlayer)
import           Prelude                                                 hiding
                                                                         (floor,
                                                                          take)

-- Import semantic wrappers - DirectionalStimulus versions
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus     (bedroomDS,
                                                                          chairDS,
                                                                          floorDS,
                                                                          pocketDS,
                                                                          robeDS)

-- Import adjectives and determiners
import           Model.Parser.Composites.Nouns                           (ConsumableNounPhrase (ConsumableNounPhrase),
                                                                          NounPhrase (SimpleNounPhrase),
                                                                          ObjectPhrase (ObjectPhrase))

-- Import behavior management constructors and spatial relationships
import           Model.GameState                                         (AcquisitionActionF,
                                                                          ActionEffectKey,
                                                                          ActionManagement (AAManagementKey, AVManagementKey, DSAManagementKey, ISAManagementKey, SAConManagementKey, SSAManagementKey),
                                                                          ContainerAccessActionF,
                                                                          DirectionalStimulusActionF,
                                                                          Effect,
                                                                          EffectActionKey (SomaticAccessActionKey),
                                                                          GameState,
                                                                          ImplicitStimulusActionF,
                                                                          Location,
                                                                          Object,
                                                                          Player,
                                                                          PlayerKey (PlayerKeyObject),
                                                                          SomaticAccessActionF,
                                                                          SpatialRelationship (ContainedIn, SupportedBy, Supports))
import           Model.GameState.GameStateDSL                            (WorldDSL,
                                                                          createAcquisitionVerbEffect,
                                                                          createAcquisitionVerbPhraseEffect,
                                                                          createDirectionalStimulusEffect,
                                                                          createImplicitStimulusEffect,
                                                                          declareAcquisitionActionGID,
                                                                          declareContainerAccessActionGID,
                                                                          declareDirectionalStimulusActionGID,
                                                                          declareImplicitStimulusActionGID,
                                                                          declareLocationGID,
                                                                          declareObjectGID,
                                                                          declareSomaticActionGID,
                                                                          finalizeGameState,
                                                                          linkEffectToLocation,
                                                                          linkEffectToObject,
                                                                          linkEffectToPlayer,
                                                                          registerLocation,
                                                                          registerObject,
                                                                          registerObjectToLocation,
                                                                          registerPlayer,
                                                                          registerSpatial,
                                                                          withDescription,
                                                                          withDescriptives,
                                                                          withLocationBehavior,
                                                                          withObjectBehavior,
                                                                          withPlayerBehavior,
                                                                          withPlayerLocation,
                                                                          withShortName,
                                                                          withTitle)
-- Import verb functions
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb    (inventory,
                                                                          isaLook)
import           Grammar.Parser.Partitions.Verbs.SomaticAccessVerbs      (saOpen)

-- Import verb phrases
import           Relude.Function                                         ((&))

-- Import action functions from BedPuzzle
import           Build.BedPuzzle.Actions.Get                             (getDeniedF,
                                                                          getF)
import           Build.BedPuzzle.Actions.Get.Constructors                (getFromSupportF,
                                                                          getObjectF)
import           Build.BedPuzzle.Actions.Inventory                       (notEvenInventoryF)
import           Build.BedPuzzle.Actions.Locations.Look                  (lookF,
                                                                          pitchBlackF)
import           Build.BedPuzzle.Actions.Look                            (lookAtF)
import           Build.BedPuzzle.Actions.Objects.Chair.Look              (whatChairF)
import           Build.BedPuzzle.Actions.Objects.Pocket.Open             (pocketOutOfReachF)
import           Build.BedPuzzle.Actions.Objects.Robe.Get                (getRobeDeniedF)
import           Build.BedPuzzle.Actions.Objects.Robe.Look               (notEvenRobeF)
import           Build.BedPuzzle.Actions.Open                            (openEyes)
import           Build.BedPuzzle.Actions.Player.Look                     (dsvActionEnabled,
                                                                          isvActionEnabled)
import           Build.BedPuzzle.Actions.Player.Open                     (openDeniedF)
import           Control.Monad                                           ((>=>))
import           Data.Kind                                               (Type)
import qualified Data.Set
import           Data.Text                                               (Text)
import           Grammar.Parser.Partitions.Nouns.Consumables             (pillCS)
import           Grammar.Parser.Partitions.Nouns.Objectives              (chairOB,
                                                                          floorOB,
                                                                          mailOB,
                                                                          robeOB)
import           Grammar.Parser.Partitions.Nouns.Surfaces                (chairSF)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs        (get)
import           Grammar.Parser.Partitions.Verbs.ConsumptionVerbs        (takeCV)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (dsaLook,
                                                                          look)
import           Grammar.Parser.Partitions.Verbs.SimpleAccessVerbs       (openSA)
import           Model.GID                                               (GID)
import           Model.Parser.Atomics.Nouns                              (DirectionalStimulus,
                                                                          Objective,
                                                                          Surface)
import           Model.Parser.Composites.Verbs                           (AcquisitionVerbPhrase (SimpleAcquisitionVerbPhrase),
                                                                          ConsumptionVerbPhrase (ConsumptionVerbPhrase))
import           Model.Parser.GCase                                      (NounKey (DirectionalStimulusKey, ObjectiveKey, SurfaceKey))
-- =============================================================================
-- VERB PHRASES
-- =============================================================================

takePillCVP :: ConsumptionVerbPhrase
takePillCVP = ConsumptionVerbPhrase takeCV (ConsumableNounPhrase (SimpleNounPhrase pillCS))

getMailAVP :: AcquisitionVerbPhrase
getMailAVP = SimpleAcquisitionVerbPhrase get (ObjectPhrase (SimpleNounPhrase mailOB))

getRobeAVP :: AcquisitionVerbPhrase-- Objects are built and contribute their actions to ActionMaps
getRobeAVP = SimpleAcquisitionVerbPhrase get (ObjectPhrase (SimpleNounPhrase robeOB))

type ObjectSpec :: Type
type ObjectSpec =
  ( GID Object                     -- The object's GID
  , WorldDSL Object                -- Builder with behaviors
  )

type EffectSpec :: Type
type EffectSpec =
  ( EffectActionKey      -- Trigger: what action causes this
  , ActionEffectKey      -- Target: LocationKey | ObjectKey | PlayerKey
  , WorldDSL Effect      -- The effect builder
  )

type ObjectBuildError :: Type
data ObjectBuildError
  = MissingAction Text
  | InvalidActionGID Text
  deriving stock (Show,Ord, Eq)

sashaBedroomDemo :: WorldDSL GameState
sashaBedroomDemo = do
  -- Location
  bedroomGID <- declareLocationGID (SimpleNounPhrase bedroomDS)
  pitchBlackGID <- declareImplicitStimulusActionGID pitchBlackF
  registerLocation bedroomGID (buildLocation pitchBlackGID)

  floorGID <- declareObjectGID (SimpleNounPhrase floorDS)
  lookAtFloorFGID <- declareDirectionalStimulusActionGID (lookAtF floorGID)
  notEvenFloorFGID <- declareDirectionalStimulusActionGID notEvenRobeF
  registerObject floorGID (floorObj notEvenFloorFGID)
  placeObject bedroomGID floorGID floorDS floorOB

  -- Chair
  chairGID <- declareObjectGID (SimpleNounPhrase chairDS)
  lookAtChairGID <- declareDirectionalStimulusActionGID (lookAtF chairGID)
  whatChairFGID <- declareDirectionalStimulusActionGID whatChairF
  getFromChairGID <- declareAcquisitionActionGID (getFromSupportF chairGID)
  registerObject chairGID (chairObj whatChairFGID getFromChairGID)
  placeObject bedroomGID chairGID chairDS chairOB
  placeSurface bedroomGID chairGID chairSF

  -- Robe
  robeGID <- declareObjectGID (SimpleNounPhrase robeDS)
  lookAtRobeFGID <- declareDirectionalStimulusActionGID (lookAtF robeGID)
  notEvenRobeFGID <- declareDirectionalStimulusActionGID notEvenRobeF
  getRobeDeniedGID <- declareAcquisitionActionGID getRobeDeniedF
  getRobeFGID <- declareAcquisitionActionGID (getObjectF robeGID)
  registerObject robeGID (robeObj notEvenRobeFGID getRobeDeniedGID)
  placeObject bedroomGID robeGID robeDS robeOB

  pocketGID <- declareObjectGID (SimpleNounPhrase pocketDS)
  lookAtPocketGID <- declareDirectionalStimulusActionGID (lookAtF pocketGID)
  openPocketNoReachGID <- declareContainerAccessActionGID pocketOutOfReachF
  registerObject pocketGID (pocketObj lookAtPocketGID openPocketNoReachGID)

  registerSpatial chairGID (Supports (Data.Set.singleton robeGID))
  registerSpatial chairGID (SupportedBy floorGID)
  registerSpatial robeGID (SupportedBy chairGID)
  registerSpatial pocketGID (ContainedIn robeGID)

  openEyesGID <- declareSomaticActionGID openEyes
  getDeniedFGID <- declareAcquisitionActionGID getDeniedF
  playerGetFGID <- declareAcquisitionActionGID getF
  lookFGID <- declareImplicitStimulusActionGID lookF
  inventoryFGID <- declareImplicitStimulusActionGID notEvenInventoryF
  isaEnabledLookGID <- declareImplicitStimulusActionGID (isvActionEnabled isaLook)
  dsvEnabledLookGID <- declareDirectionalStimulusActionGID dsvActionEnabled
  containerAccessDeniedFGID <- declareContainerAccessActionGID openDeniedF

  player <- buildBedroomPlayer
              bedroomGID
              isaEnabledLookGID
              inventoryFGID
              openEyesGID
              dsvEnabledLookGID
              getDeniedFGID
              containerAccessDeniedFGID
  registerPlayer player

  -- Effects
--  change look for Location
  openEyesLookChangeEffectPlayer <- createImplicitStimulusEffect isaLook lookFGID
  linkEffectToLocation (SomaticAccessActionKey openEyesGID) bedroomGID openEyesLookChangeEffectPlayer
  -- change look for FLOOR
  openEyesLookChangeEffectFloor <- createDirectionalStimulusEffect dsaLook lookAtFloorFGID
  linkEffectToObject (SomaticAccessActionKey openEyesGID) floorGID openEyesLookChangeEffectFloor

  -- change look for Chair
  openeEyesLooKChangeEffectChair <- createDirectionalStimulusEffect dsaLook lookAtChairGID
  linkEffectToObject (SomaticAccessActionKey openEyesGID) chairGID openeEyesLooKChangeEffectChair

  -- change look for ROBE
  openEyesLookChangeEffectRobe <- createDirectionalStimulusEffect dsaLook lookAtRobeFGID
  linkEffectToObject (SomaticAccessActionKey openEyesGID) robeGID openEyesLookChangeEffectRobe

  -- change get robe for Player
  robeOpenEyesLookChangesGetRobeForPlayer <- createAcquisitionVerbPhraseEffect getRobeAVP playerGetFGID
  linkEffectToPlayer (SomaticAccessActionKey openEyesGID) (PlayerKeyObject robeGID) robeOpenEyesLookChangesGetRobeForPlayer

 -- change get robe from chair for ROBE
  robeOpenEyesLookChangesGetRobePhraseForRobe <- createAcquisitionVerbPhraseEffect getRobeAVP getRobeFGID
  linkEffectToObject (SomaticAccessActionKey openEyesGID) robeGID robeOpenEyesLookChangesGetRobePhraseForRobe

  -- change get robe for Robe
  robeOpenEyesLookChangesGetRobeForRobe <- createAcquisitionVerbEffect get getRobeFGID
  linkEffectToObject (SomaticAccessActionKey openEyesGID) robeGID robeOpenEyesLookChangesGetRobeForRobe

--  registerSpatial tableGID (Supports (Set.singleton mailGID))
--  registerSpatial tableGID (SupportedBy floorGID)
--  registerSpatial mailGID (SupportedBy tableGID)
--  registerSpatial robeGID (Contains (Data.Set.singleton pocketGID))
--  registerSpatial pocketGID (Contains (Set.singleton pillGID))
--  registerSpatial pillGID (ContainedIn pocketGID)
--  registerSpatial floorGID (Supports (Data.Set.fromList [chairGID, tableGID]))

  finalizeGameState

buildLocation :: GID ImplicitStimulusActionF -> WorldDSL Location
buildLocation implicitLookResponseGID = defaultLocation & bedroomLoc'
  where
    bedroomLoc' = withTitle "bedroom in bed"
                    >=> (\l -> withLocationBehavior l (ISAManagementKey isaLook implicitLookResponseGID))

buildBedroomPlayer :: GID Location
                        -> GID ImplicitStimulusActionF
                        -> GID ImplicitStimulusActionF
                        -> GID SomaticAccessActionF
                        -> GID DirectionalStimulusActionF
                        -> GID AcquisitionActionF
                        -> GID ContainerAccessActionF
                        -> WorldDSL Player
buildBedroomPlayer bedroomGID implicitLookResponseGID inventoryFGID openEyesGID directLookResponseGID getRobeFGID containerAccessDeniedF =
  withPlayerLocation defaultPlayer bedroomGID
    >>= (\p -> withPlayerBehavior p (ISAManagementKey isaLook implicitLookResponseGID))
    >>= (\p -> withPlayerBehavior p (ISAManagementKey inventory inventoryFGID))
    >>= (\p -> withPlayerBehavior p (DSAManagementKey look directLookResponseGID))
    >>= (\p -> withPlayerBehavior p (SSAManagementKey saOpen openEyesGID))
    >>= (\p -> withPlayerBehavior p (AAManagementKey getRobeAVP getRobeFGID))
    >>= (\p -> withPlayerBehavior p (AVManagementKey get getRobeFGID))
    >>= (\p -> withPlayerBehavior p (SAConManagementKey openSA containerAccessDeniedF))

bedroomLoc :: GID ImplicitStimulusActionF ->  WorldDSL Location
bedroomLoc lookResponseGID = defaultLocation & bedroomLoc'
  where
    bedroomLoc' = withTitle "bedroom in bed"
                    >=> (\o -> withLocationBehavior o (ISAManagementKey isaLook lookResponseGID))

floorObj :: GID DirectionalStimulusActionF
         -> WorldDSL Object
floorObj lookGID =  defaultObject & floor
  where
    floor =
      withShortName "floor"
        >=> withDescription "The bedroom floor"
        >=> withDescriptives [SimpleNounPhrase floorDS]
        >=> (\o -> withObjectBehavior o (DSAManagementKey look lookGID))

chairObj :: GID DirectionalStimulusActionF
         -> GID AcquisitionActionF
         -> WorldDSL Object
chairObj lookGID getGID =  defaultObject & chair
  where
    chair =
      withShortName "chair"
        >=> withDescription "A simple wooden chair"
        >=> withDescriptives [SimpleNounPhrase chairDS]
        >=> (\o -> withObjectBehavior o (DSAManagementKey look lookGID))
        >=> (\o -> withObjectBehavior o (AVManagementKey get getGID))
        >=> (\o -> withObjectBehavior o (AAManagementKey getRobeAVP getGID))

robeObj :: GID DirectionalStimulusActionF
        -> GID AcquisitionActionF
        -> WorldDSL Object
robeObj lookGID getGID = defaultObject & robe
  where
    robe =
      withShortName "comfortable robe"
        >=> withDescription "The robe is draped on the chair"
        >=> withDescriptives [SimpleNounPhrase robeDS]
        >=> (\o -> withObjectBehavior o (DSAManagementKey look lookGID))
        >=> (\o -> withObjectBehavior o (AVManagementKey get getGID))
        >=> (\o -> withObjectBehavior o (AAManagementKey getRobeAVP getGID))



pocketObj :: GID DirectionalStimulusActionF
          -> GID ContainerAccessActionF
          -> WorldDSL Object
pocketObj lookGID openGID = defaultObject & pocket
  where
    pocket =
      withShortName "pocket"
        >=> withDescription "A pocket sewn into the robe"
        >=> withDescriptives [SimpleNounPhrase pocketDS]
        >=> (\o -> withObjectBehavior o (DSAManagementKey look lookGID))
        >=> (\o -> withObjectBehavior o (SAConManagementKey openSA openGID))

placeObject :: GID Location -> GID Object -> DirectionalStimulus -> Objective -> WorldDSL ()
placeObject lid oid ds obj = do
  registerObjectToLocation lid oid (DirectionalStimulusKey ds)
  registerObjectToLocation lid oid (ObjectiveKey obj)

placeSurface :: GID Location -> GID Object -> Surface -> WorldDSL ()
placeSurface lid oid surface = do
  registerObjectToLocation lid oid (SurfaceKey surface)

