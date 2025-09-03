{-# LANGUAGE OverloadedStrings #-}

module Build.GameStateGeneration.TestDynamicActions where

import           Build.GameStateGeneration.Defaults                               (defaultLocation,
                                                                                   defaultObject,
                                                                                   defaultPlayer)
import           Prelude                                                          hiding
                                                                                  (take)

-- Import semantic wrappers - DirectionalStimulus versions
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus              (bedroomDS,
                                                                                   chairDS,
                                                                                   floorDS,
                                                                                   robeDS)

-- Import adjectives and determiners
import           Model.Parser.Composites.Nouns                                    (ConsumableNounPhrase (ConsumableNounPhrase),
                                                                                   DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                                                   NounPhrase (SimpleNounPhrase),
                                                                                   ObjectPhrase (ObjectPhrase))

-- Import behavior management constructors and spatial relationships
import           Model.GameState                                                  (AcquisitionActionF,
                                                                                   ActionManagement (AAManagementKey, AVManagementKey, DSAManagementKey, ISAManagementKey, SSAManagementKey),
                                                                                   DirectionalStimulusActionF,
                                                                                   EffectActionKey (SomaticAccessActionKey),
                                                                                   GameState,
                                                                                   ImplicitStimulusActionF,
                                                                                   Location,
                                                                                   Object,
                                                                                   Player,
                                                                                   PlayerKey (PlayerKeyObject),
                                                                                   SomaticAccessActionF,
                                                                                   SpatialRelationship (SupportedBy, Supports))
import           Model.GameState.GameStateDSL                                     (WorldDSL,
                                                                                   createAcquisitionVerbEffect,
                                                                                   createAcquisitionVerbPhraseEffect,
                                                                                   createDirectionalStimulusEffect,
                                                                                   createImplicitStimulusEffect,
                                                                                   declareAcquisitionActionGID,
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
                                                                                   setPerceptionMap,
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
import           Build.BedPuzzle.Actions.Objects.Robe.Get                         (getRobeDeniedF)
import           Build.BedPuzzle.Actions.Objects.Robe.Look                        (notEvenRobeF)
import           Build.BedPuzzle.Actions.Open                                     (openEyes)
import           Build.BedPuzzle.Actions.Player.Look                              (dsvActionEnabled,
                                                                                   isvActionEnabled)
import           Control.Monad                                                    ((>=>))
import qualified Data.Set
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

getRobeAVP :: AcquisitionVerbPhrase
getRobeAVP = SimpleAcquisitionVerbPhrase get (ObjectPhrase (SimpleNounPhrase robeOB))

testDynamicActionsDSL :: WorldDSL GameState
testDynamicActionsDSL = do
  -- Declare location GID
  bedroomGID <- declareLocationGID (SimpleNounPhrase bedroomDS)

-- Object GIDs
  floorGID <- declareObjectGID (SimpleNounPhrase floorDS)
  chairGID <- declareObjectGID (SimpleNounPhrase chairDS)
  robeGID <- declareObjectGID (SimpleNounPhrase robeDS)

  lookAtChairFGID <- declareDirectionalStimulusActionGID (lookAtF chairGID)
  whatChairGID <- declareDirectionalStimulusActionGID whatChairF

  lookAtRobeFGID <- declareDirectionalStimulusActionGID (lookAtF robeGID)
  notEvenRobeFGID <- declareDirectionalStimulusActionGID notEvenRobeF
  getRobeDeniedFGID <- declareAcquisitionActionGID getRobeDeniedF
  getRobeFGID <- declareAcquisitionActionGID (getObjectF robeGID)
  getFromChairFGID <- declareAcquisitionActionGID (getFromSupportF chairGID)
  lookFloorGID <- declareDirectionalStimulusActionGID (lookAtF floorGID)

  whatPillFGID <- declareDirectionalStimulusActionGID whatPill

  registerObject chairGID (chairObj whatChairGID getFromChairFGID)
  registerObject floorGID (floorObj lookFloorGID)
  registerObject robeGID (robeObj notEvenRobeFGID getRobeDeniedFGID)

  -- Generate open eyes action GIDs dynamically
  openEyesGID <- declareSomaticActionGID openEyes
  getDeniedFGID <- declareAcquisitionActionGID getDeniedF
  playerGetFGID <- declareAcquisitionActionGID getF
  pitchBlackGID <- declareImplicitStimulusActionGID pitchBlackF
  lookFGID <- declareImplicitStimulusActionGID lookF
  isaEnabledLookGID <- declareImplicitStimulusActionGID (isvActionEnabled isaLook)
  dsvEnabledLookGID <- declareDirectionalStimulusActionGID dsvActionEnabled

  registerLocation bedroomGID (buildLocation pitchBlackGID)

  placeObject bedroomGID chairGID chairDS chairOB
  placeObject bedroomGID floorGID floorDS floorOB
  placeObject bedroomGID robeGID robeDS robeOB

  registerSpatial chairGID (SupportedBy floorGID)
  registerSpatial floorGID (Supports (Data.Set.singleton chairGID))
  registerSpatial chairGID (Supports (Data.Set.singleton robeGID))
  registerSpatial robeGID (SupportedBy chairGID)
--  registerObject floorGID (floorObj id)
  player <- buildBedroomPlayer bedroomGID isaEnabledLookGID openEyesGID dsvEnabledLookGID getDeniedFGID

  openEyesLookChangeEffect <- createImplicitStimulusEffect isaLook lookFGID
  linkEffectToLocation (SomaticAccessActionKey openEyesGID) bedroomGID openEyesLookChangeEffect

  openEyesLookChangesLookChair <- createDirectionalStimulusEffect look lookAtChairFGID
  linkEffectToObject (SomaticAccessActionKey openEyesGID) chairGID openEyesLookChangesLookChair

  robeOpenEyesLookChangesLookRobe <- createDirectionalStimulusEffect look lookAtRobeFGID
  linkEffectToObject ( SomaticAccessActionKey openEyesGID) robeGID robeOpenEyesLookChangesLookRobe

  openEyesLookChangesGetRobe <- createAcquisitionVerbPhraseEffect getRobeAVP getRobeFGID
  linkEffectToObject ( SomaticAccessActionKey openEyesGID) robeGID openEyesLookChangesGetRobe

  robeOpenEyesLookChangesGetRobeForPlayer <- createAcquisitionVerbPhraseEffect getRobeAVP playerGetFGID
  linkEffectToPlayer (SomaticAccessActionKey openEyesGID) (PlayerKeyObject robeGID) robeOpenEyesLookChangesGetRobeForPlayer

  openEyesChangeGetVerb <- createAcquisitionVerbEffect get getRobeFGID
  linkEffectToObject (SomaticAccessActionKey openEyesGID) robeGID openEyesChangeGetVerb

  registerPlayer player
  setPerceptionMap
    [ (DirectionalStimulusNounPhrase at (SimpleNounPhrase chairDS), [chairGID])
--    , (DirectionalStimulusNounPhrase (SimpleNounPhrase tableDS), [tableGID])
    , (DirectionalStimulusNounPhrase at (SimpleNounPhrase robeDS), [robeGID])
--    , (DirectionalStimulusNounPhrase (SimpleNounPhrase mailDS), [mailGID])
    , (DirectionalStimulusNounPhrase at (SimpleNounPhrase floorDS), [floorGID])
    ]

  finalizeGameState

buildLocation :: GID ImplicitStimulusActionF -> WorldDSL Location
buildLocation pitchBlackGID = defaultLocation & bedroomLoc'
  where
    bedroomLoc' = withTitle "bedroom in bed"
                    >=> (\l -> withLocationBehavior l (ISAManagementKey isaLook pitchBlackGID))

buildBedroomPlayer :: GID Location
                        -> GID ImplicitStimulusActionF
                        -> GID SomaticAccessActionF
                        -> GID DirectionalStimulusActionF
                        -> GID AcquisitionActionF
                        -> WorldDSL Player
buildBedroomPlayer bedroomGID isaEnabledLookGID openEyesGID dsaEnabledLookGID getRobeFGID =
  withPlayerLocation defaultPlayer bedroomGID
    >>= (\p -> withPlayerBehavior p (ISAManagementKey isaLook isaEnabledLookGID))
    >>= (\p -> withPlayerBehavior p (DSAManagementKey look dsaEnabledLookGID))
    >>= (\p -> withPlayerBehavior p (SSAManagementKey saOpen openEyesGID))
    >>= (\p -> withPlayerBehavior p (ISAManagementKey isaLook isaEnabledLookGID))
    >>= (\p -> withPlayerBehavior p (AAManagementKey getRobeAVP getRobeFGID))
    >>= (\p -> withPlayerBehavior p (AVManagementKey get getRobeFGID))

robeObj :: GID DirectionalStimulusActionF  -- Eyes closed look response
        -> GID AcquisitionActionF          -- Can't get (eyes closed/not standing)
        -> WorldDSL Object
robeObj cannotSeeRobeGID cannotGetRobeGID =
  defaultObject & robeObj'
  where
    robeObj' = withShortName "comfortable robe"
                 >=> withDescription "The robe is draped on the chair"
                 >=> withDescriptives [SimpleNounPhrase robeDS]
                 >=> (\o -> withObjectBehavior o (DSAManagementKey look cannotSeeRobeGID))
                 >=> (\o -> withObjectBehavior o (AVManagementKey get cannotGetRobeGID))
                 >=> (\o -> withObjectBehavior o (AAManagementKey getRobeAVP cannotGetRobeGID))

chairObj :: GID DirectionalStimulusActionF
              -> GID AcquisitionActionF
              -> WorldDSL Object
chairObj lookResponseGID getResponseFGID = defaultObject & chairObj'
  where
    chairObj' = withShortName "chair"
                  >=> withDescription "A simple wooden chair"
                  >=> withDescriptives [SimpleNounPhrase chairDS]
                  >=> (\o -> withObjectBehavior o (DSAManagementKey look lookResponseGID))
                  >=> (\o -> withObjectBehavior o (AVManagementKey  get  getResponseFGID))


placeObject :: GID Location -> GID Object -> DirectionalStimulus -> Objective -> WorldDSL ()
placeObject lid oid ds obj = do
  registerObjectToLocation lid oid (DirectionalStimulusKey ds)
  registerObjectToLocation lid oid (ObjectiveKey obj)

floorObj :: GID DirectionalStimulusActionF -> WorldDSL Object
floorObj lookFloorGID = defaultObject & floorObj'
  where
    floorObj' = withShortName "floor"
                  >=> withDescription "The bedroom floor"
                  >=> withDescriptives [SimpleNounPhrase floorDS]
                  >=> (\o -> withObjectBehavior o (DSAManagementKey look lookFloorGID))
