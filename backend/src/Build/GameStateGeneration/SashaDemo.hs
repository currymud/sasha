{-# LANGUAGE OverloadedStrings #-}

module Build.GameStateGeneration.SashaDemo where

import           Build.GameStateGeneration.Defaults                               (defaultLocation,
                                                                                   defaultObject,
                                                                                   defaultPlayer)
import           Prelude                                                          hiding
                                                                                  (floor,
                                                                                   take)

-- Import semantic wrappers - DirectionalStimulus versions
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus              (bedroomDS,
                                                                                   chairDS,
                                                                                   floorDS,
                                                                                   pocketDS,
                                                                                   robeDS)

-- Import adjectives and determiners
import           Model.Parser.Composites.Nouns                                    (ConsumableNounPhrase (ConsumableNounPhrase),
                                                                                   DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                                                   NounPhrase (SimpleNounPhrase),
                                                                                   ObjectPhrase (ObjectPhrase))

-- Import behavior management constructors and spatial relationships
import           Model.GameState                                                  (AcquisitionActionF,
                                                                                   ActionEffectKey,
                                                                                   ActionManagement (AAManagementKey, AVManagementKey, DSAManagementKey, ISAManagementKey, SAConManagementKey, SSAManagementKey),
                                                                                   ActionManagementFunctions (ActionManagementFunctions),
                                                                                   ContainerAccessActionF,
                                                                                   DirectionalStimulusActionF,
                                                                                   Effect,
                                                                                   EffectActionKey (AcquisitionalActionKey, SomaticAccessActionKey),
                                                                                   GameState,
                                                                                   ImplicitStimulusActionF,
                                                                                   Location,
                                                                                   Object,
                                                                                   Player,
                                                                                   PlayerKey (PlayerKeyObject),
                                                                                   SomaticAccessActionF,
                                                                                   SpatialRelationship (ContainedIn, SupportedBy, Supports))
import           Model.GameState.GameStateDSL                                     (WorldDSL,
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
                                                                                   linkFieldEffectToObject,
                                                                                   registerLocation,
                                                                                   registerObject,
                                                                                   registerObjectToLocation,
                                                                                   registerPlayer,
                                                                                   registerSpatial,
                                                                                   setPerceptionMap,
                                                                                   updateDescription,
                                                                                   withDescription,
                                                                                   withDescriptives,
                                                                                   withLocationBehavior,
                                                                                   withObjectBehavior,
                                                                                   withPlayerBehavior,
                                                                                   withPlayerLocation,
                                                                                   withShortName,
                                                                                   withTitle)
-- Import verb functions
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb             (isaLook)
import           Grammar.Parser.Partitions.Verbs.SomaticAccessVerbs               (saOpen)

-- Import verb phrases
import           Relude.Function                                                  ((&))

-- Import action functions from BedPuzzle
import           Build.BedPuzzle.Actions.Get                                      (getDeniedF,
                                                                                   getF)
import           Build.BedPuzzle.Actions.Get.Constructors                         (getFromSupportF,
                                                                                   getObjectF)
import           Build.BedPuzzle.Actions.Locations.Look                           (lookF,
                                                                                   pitchBlackF)
import           Build.BedPuzzle.Actions.Look                                     (lookAtF)
import           Build.BedPuzzle.Actions.Objects.Chair.Look                       (whatChairF)
import           Build.BedPuzzle.Actions.Objects.Pill.Look                        (whatPill)
import           Build.BedPuzzle.Actions.Objects.Pocket.Look                      (whatPocket)
import           Build.BedPuzzle.Actions.Objects.Pocket.Open                      (pocketOutOfReachF)
import           Build.BedPuzzle.Actions.Objects.Robe.Get                         (getRobeDeniedF)
import           Build.BedPuzzle.Actions.Objects.Robe.Look                        (notEvenRobeF)
import           Build.BedPuzzle.Actions.Open                                     (openEyes)
import           Build.BedPuzzle.Actions.Player.Look                              (dsvActionEnabled,
                                                                                   isvActionEnabled)
import           Build.BedPuzzle.Actions.Player.Open                              (openDeniedF)
import           Build.GameStateGeneration.TestDynamicActions                     (placeObject)
import           Control.Monad                                                    ((>=>))
import           Data.Kind                                                        (Type)
import qualified Data.Set
import           Data.Text                                                        (Text)
import           Debug.Trace                                                      (trace)
import           GameState.ActionManagement                                       (findAVKey,
                                                                                   findDSAKey,
                                                                                   findSAForContainersKey)
import           Grammar.Parser.Partitions.Nouns.Consumables                      (pill,
                                                                                   pillCS)
import           Grammar.Parser.Partitions.Nouns.Objectives                       (chairOB,
                                                                                   floorOB,
                                                                                   mailOB,
                                                                                   pillOB,
                                                                                   robeOB)
import           Grammar.Parser.Partitions.Prepositions.DirectionalStimulusMarker (at)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs                 (get,
                                                                                   take)
import           Grammar.Parser.Partitions.Verbs.ConsumptionVerbs                 (takeCV)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb          (dsaLook,
                                                                                   look)
import           Grammar.Parser.Partitions.Verbs.SimpleAccessVerbs                (openSA)
import           Model.GID                                                        (GID)
import           Model.Parser.Atomics.Nouns                                       (DirectionalStimulus,
                                                                                   Objective)
import           Model.Parser.Composites.Verbs                                    (AcquisitionVerbPhrase (SimpleAcquisitionVerbPhrase),
                                                                                   ConsumptionVerbPhrase (ConsumptionVerbPhrase))
import           Model.Parser.GCase                                               (NounKey (DirectionalStimulusKey, ObjectiveKey))
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

  floorGID <- declareObjectGID (SimpleNounPhrase floorDS)
  lookAtFloorGID <- declareDirectionalStimulusActionGID (lookAtF floorGID)
  registerObject floorGID (floorObj lookAtFloorGID)
  placeObject bedroomGID floorGID floorDS floorOB

  -- Chair
  chairGID <- declareObjectGID (SimpleNounPhrase chairDS)
  lookAtChairGID <- declareDirectionalStimulusActionGID (lookAtF chairGID)
  getFromChairGID <- declareAcquisitionActionGID (getFromSupportF chairGID)
  registerObject chairGID (chairObj lookAtChairGID getFromChairGID)
  placeObject bedroomGID chairGID chairDS chairOB

  -- Robe
  robeGID <- declareObjectGID (SimpleNounPhrase robeDS)
  lookAtRobeGID <- declareDirectionalStimulusActionGID (lookAtF robeGID)
  getRobeDeniedGID <- declareAcquisitionActionGID getRobeDeniedF
  registerObject robeGID (robeObj lookAtRobeGID getRobeDeniedGID)
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
  pitchBlackGID <- declareImplicitStimulusActionGID pitchBlackF
  lookFGID <- declareImplicitStimulusActionGID lookF
  isaEnabledLookGID <- declareImplicitStimulusActionGID (isvActionEnabled isaLook)
  dsvEnabledLookGID <- declareDirectionalStimulusActionGID dsvActionEnabled
  containerAccessDeniedFGID <- declareContainerAccessActionGID openDeniedF
  registerLocation bedroomGID (buildLocation pitchBlackGID)

  player <- buildBedroomPlayer bedroomGID isaEnabledLookGID openEyesGID dsvEnabledLookGID getDeniedFGID containerAccessDeniedFGID
  registerPlayer player

  getRobeGID <- declareAcquisitionActionGID (getObjectF robeGID)
  -- Effects
  openEyesLookChangeEffect <- createImplicitStimulusEffect isaLook lookFGID
  linkEffectToLocation (SomaticAccessActionKey openEyesGID) bedroomGID openEyesLookChangeEffect

  robeOpenEyesLookChangesGetRobeForPlayer <- createAcquisitionVerbPhraseEffect getRobeAVP playerGetFGID
  linkEffectToPlayer (SomaticAccessActionKey openEyesGID) (PlayerKeyObject robeGID) robeOpenEyesLookChangesGetRobeForPlayer
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
                        -> GID SomaticAccessActionF
                        -> GID DirectionalStimulusActionF
                        -> GID AcquisitionActionF
                        -> GID ContainerAccessActionF
                        -> WorldDSL Player
buildBedroomPlayer bedroomGID implicitLookResponseGID openEyesGID directLookResponseGID getRobeFGID containerAccessDeniedF =
  withPlayerLocation defaultPlayer bedroomGID
    >>= (\p -> withPlayerBehavior p (ISAManagementKey isaLook implicitLookResponseGID))
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
