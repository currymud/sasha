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
import           Model.Parser.Composites.Nouns                                    (DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                                                   NounPhrase (SimpleNounPhrase))

-- Import behavior management constructors and spatial relationships
import           Model.GameState                                                  (AcquisitionActionF,
                                                                                   ActionManagement (AVManagementKey, DSAManagementKey, ISAManagementKey, SSAManagementKey),
                                                                                   DirectionalStimulusActionF,
                                                                                   EffectActionKey (SomaticAccessActionKey),
                                                                                   GameState,
                                                                                   ImplicitStimulusActionF,
                                                                                   Location,
                                                                                   Object,
                                                                                   Player,
                                                                                   SomaticAccessActionF,
                                                                                   SpatialRelationship (SupportedBy, Supports))
import           Model.GameState.GameStateDSL                                     (WorldDSL,
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
import           Build.BedPuzzle.Actions.Get                                      (getF)
import           Build.BedPuzzle.Actions.Get.Constructors                         (getFromSupportF)
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
import           Grammar.Parser.Partitions.Nouns.Objectives                       (chairOB,
                                                                                   floorOB,
                                                                                   robeOB)
import           Grammar.Parser.Partitions.Prepositions.DirectionalStimulusMarker (at)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs                 (get)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb          (dsaLook,
                                                                                   look)
import           Model.GID                                                        (GID)
import           Model.Parser.Atomics.Nouns                                       (DirectionalStimulus,
                                                                                   Objective)
import           Model.Parser.GCase                                               (NounKey (DirectionalStimulusKey, ObjectiveKey))


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
  getRobeFGID <- declareAcquisitionActionGID (getFromSupportF robeGID)

  lookFloorGID <- declareDirectionalStimulusActionGID (lookAtF floorGID)

  whatPillFGID <- declareDirectionalStimulusActionGID whatPill

  registerObject chairGID (chairObj whatChairGID)
  registerObject floorGID (floorObj lookFloorGID)
  registerObject robeGID (robeObj notEvenRobeFGID getRobeDeniedFGID)

  -- Generate open eyes action GIDs dynamically
  openEyesGID <- declareSomaticActionGID openEyes
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
  registerSpatial robeGID (SupportedBy chairGID)
--  registerObject floorGID (floorObj id)
  player <- buildBedroomPlayer bedroomGID isaEnabledLookGID openEyesGID dsvEnabledLookGID

  openEyesLookChangeEffect <- createImplicitStimulusEffect isaLook lookFGID
  linkEffectToLocation (SomaticAccessActionKey openEyesGID) bedroomGID openEyesLookChangeEffect

  openEyesLookChangesLookChair <- createDirectionalStimulusEffect look lookAtChairFGID
  linkEffectToObject (SomaticAccessActionKey openEyesGID) chairGID openEyesLookChangesLookChair

  robeOpenEyesLookChangesLookRobe <- createDirectionalStimulusEffect look lookAtRobeFGID
  linkEffectToObject ( SomaticAccessActionKey openEyesGID) robeGID robeOpenEyesLookChangesLookRobe


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
                        -> WorldDSL Player
buildBedroomPlayer bedroomGID isaEnabledLookGID openEyesGID dsaEnabledLookGID =
  withPlayerLocation defaultPlayer bedroomGID
    >>= (\p -> withPlayerBehavior p (ISAManagementKey isaLook isaEnabledLookGID))
    >>= (\p -> withPlayerBehavior p (DSAManagementKey look dsaEnabledLookGID))
    >>= (\p -> withPlayerBehavior p (SSAManagementKey saOpen openEyesGID))

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


chairObj :: GID DirectionalStimulusActionF -> WorldDSL Object
chairObj lookResponseGID = defaultObject & chairObj'
  where
    chairObj' = withShortName "chair"
                  >=> withDescription "A simple wooden chair"
                  >=> withDescriptives [SimpleNounPhrase chairDS]
                  >=> (\o -> withObjectBehavior o (DSAManagementKey look lookResponseGID))

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
