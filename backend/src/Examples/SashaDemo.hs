{-# LANGUAGE OverloadedStrings #-}

module Examples.SashaDemo where

import           Control.Monad                                           ((>=>))
import qualified Data.Set

-- Core imports
import           Examples.Defaults                                       (defaultLocation,
                                                                          defaultObject,
                                                                          defaultPlayer)
import           Model.Core                                              (AcquisitionActionF,
                                                                          ActionEffectKey (..),
                                                                          ActionManagement (..),
                                                                          ContainerAccessActionF,
                                                                          DirectionalStimulusActionF,
                                                                          GameState,
                                                                          ImplicitStimulusActionF,
                                                                          Location,
                                                                          Object,
                                                                          Player,
                                                                          PlayerKey (..),
                                                                          SomaticAccessActionF,
                                                                          SpatialRelationship (..))
import           Model.EDSL.SashaDSL                                     (SashaDSL,
                                                                          createAcquisitionVerbEffect,
                                                                          createAcquisitionVerbPhraseEffect,
                                                                          createContainerAccessEffect,
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
                                                                          registerLocation,
                                                                          registerObject,
                                                                          registerObjectToLocation,
                                                                          registerPlayer,
                                                                          registerSpatial,
                                                                          withDescription,
                                                                          withDescriptives,
                                                                          withPlayerLocation,
                                                                          withShortName,
                                                                          withTitle)
import           Model.GID                                               (GID)
import           Sasha.EffectAlgebra                                     (alongside,
                                                                          andThen,
                                                                          buildEffect,
                                                                          buildEffects)
import           Sasha.HasBehavior                                       (HasBehavior (withBehavior),
                                                                          MakeBehavior (makeBehavior))
import           Sasha.HasEffect                                         (HasEffect (linkEffect),
                                                                          MakeEffect (makeEffect))

-- All the noun/verb imports from original
import           Grammar.Parser.Partitions.Nouns.Containers              (pocketCT)
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus     (bedroomDS,
                                                                          chairDS,
                                                                          floorDS,
                                                                          pocketDS,
                                                                          robeDS)
import           Grammar.Parser.Partitions.Nouns.Objectives              (chairOB,
                                                                          floorOB,
                                                                          pocketOB,
                                                                          robeOB)
import           Grammar.Parser.Partitions.Nouns.Surfaces                (chairSF)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs        (get)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (dsaLook,
                                                                          look)
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb    (inventory,
                                                                          isaLook)
import           Grammar.Parser.Partitions.Verbs.SimpleAccessVerbs       (openSA)
import           Grammar.Parser.Partitions.Verbs.SomaticAccessVerbs      (saOpen)
import           Model.Parser.Composites.Nouns                           (ContainerPhrase (ContainerPhrase),
                                                                          NounPhrase (SimpleNounPhrase),
                                                                          ObjectPhrase (ObjectPhrase))
import           Model.Parser.Composites.Verbs                           (AcquisitionVerbPhrase (SimpleAcquisitionVerbPhrase),
                                                                          ContainerAccessVerbPhrase (SimpleAccessContainerVerbPhrase))
import           Model.Parser.GCase                                      (NounKey (DirectionalStimulusKey, ObjectiveKey, SurfaceKey))

-- Action functions from original
import           ConstraintRefinement.Actions.Locations.Look             (lookF,
                                                                          pitchBlackF)
-- Narration effects
import           ConstraintRefinement.Effects.NarrationEffects            (pitchBlackNarration,
                                                                          openEyesSuccessNarration,
                                                                          eyesClosedRobeNarration,
                                                                          eyesClosedFloorNarration,
                                                                          eyesClosedChairNarration,
                                                                          pickUpNarration,
                                                                          getRobeDeniedNarration,
                                                                          pocketOutOfReachNarration)
import           ConstraintRefinement.Actions.Objects.Chair.Look         (whatChairF)
import           ConstraintRefinement.Actions.Objects.Get.Constructors   (getFromSupportF,
                                                                          getObjectF)
import           ConstraintRefinement.Actions.Objects.Pocket.Open        (pocketOutOfReachF)
import           ConstraintRefinement.Actions.Objects.Robe.Get           (getRobeDeniedF)
import           ConstraintRefinement.Actions.Objects.Robe.Look          (notEvenRobeF)
import           ConstraintRefinement.Actions.Player.Get                 (getDeniedF,
                                                                          getF)
import           ConstraintRefinement.Actions.Player.Inventory           (defaultInventoryLookF)
import           ConstraintRefinement.Actions.Player.Look                (dsvActionEnabled,
                                                                          isvActionEnabled,
                                                                          lookAtF)
import           ConstraintRefinement.Actions.Player.Open                (openDeniedF,
                                                                          openEyes,
                                                                          openF)
import           Data.Function                                           ((&))

-- Verb phrases from original
getRobeAVP :: AcquisitionVerbPhrase
getRobeAVP = SimpleAcquisitionVerbPhrase get (ObjectPhrase (SimpleNounPhrase robeOB))

openPocketCVP :: ContainerAccessVerbPhrase
openPocketCVP = SimpleAccessContainerVerbPhrase openSA (ContainerPhrase (SimpleNounPhrase pocketCT))

-- | Main demo function with HasBehavior and HasEffect - same signature as original
sashaBedroomDemo :: SashaDSL GameState
sashaBedroomDemo = do
  bedroomGID <- declareLocationGID (SimpleNounPhrase bedroomDS)
  floorGID <- declareObjectGID (SimpleNounPhrase floorDS)
  chairGID <- declareObjectGID (SimpleNounPhrase chairDS)
  robeGID <- declareObjectGID (SimpleNounPhrase robeDS)
  pocketGID <- declareObjectGID (SimpleNounPhrase pocketDS)

  pitchBlackGID <- declareImplicitStimulusActionGID pitchBlackF
  lookAtFloorFGID <- declareDirectionalStimulusActionGID (lookAtF floorGID)
  notEvenFloorFGID <- declareDirectionalStimulusActionGID notEvenRobeF
  lookAtChairGID <- declareDirectionalStimulusActionGID (lookAtF chairGID)
  whatChairFGID <- declareDirectionalStimulusActionGID whatChairF
  getFromChairGID <- declareAcquisitionActionGID (getFromSupportF chairGID)
  lookAtRobeFGID <- declareDirectionalStimulusActionGID (lookAtF robeGID)
  notEvenRobeFGID <- declareDirectionalStimulusActionGID notEvenRobeF
  getRobeDeniedGID <- declareAcquisitionActionGID getRobeDeniedF
  getRobeFGID <- declareAcquisitionActionGID (getObjectF robeGID)
  lookAtPocketGID <- declareDirectionalStimulusActionGID (lookAtF pocketGID)
  openPocketNoReachGID <- declareContainerAccessActionGID pocketOutOfReachF

  openEyesGID <- declareSomaticActionGID openEyes
  getDeniedFGID <- declareAcquisitionActionGID getDeniedF
  playerGetFGID <- declareAcquisitionActionGID getF
  lookFGID <- declareImplicitStimulusActionGID lookF
  inventoryFGID <- declareImplicitStimulusActionGID defaultInventoryLookF
  isaEnabledLookGID <- declareImplicitStimulusActionGID (isvActionEnabled isaLook)
  dsvEnabledLookGID <- declareDirectionalStimulusActionGID dsvActionEnabled
  containerAccessDeniedFGID <- declareContainerAccessActionGID openDeniedF
  accessContainerFGID <- declareContainerAccessActionGID openF

  registerLocation bedroomGID (buildLocation pitchBlackGID)
  registerObject floorGID (floorObj notEvenFloorFGID)
  registerObject chairGID (chairObj whatChairFGID getFromChairGID)
  registerObject robeGID (robeObj notEvenRobeFGID getRobeDeniedGID)
  registerObject pocketGID (pocketObj lookAtPocketGID openPocketNoReachGID)

  player <- buildBedroomPlayer bedroomGID isaEnabledLookGID inventoryFGID openEyesGID
                              dsvEnabledLookGID getDeniedFGID containerAccessDeniedFGID
  registerPlayer player

  registerObjectToLocation bedroomGID floorGID (DirectionalStimulusKey floorDS)
  registerObjectToLocation bedroomGID floorGID (ObjectiveKey floorOB)
  registerObjectToLocation bedroomGID chairGID (DirectionalStimulusKey chairDS)
  registerObjectToLocation bedroomGID chairGID (ObjectiveKey chairOB)
  registerObjectToLocation bedroomGID chairGID (SurfaceKey chairSF)
  registerObjectToLocation bedroomGID robeGID (DirectionalStimulusKey robeDS)
  registerObjectToLocation bedroomGID robeGID (ObjectiveKey robeOB)
  registerObjectToLocation bedroomGID pocketGID (DirectionalStimulusKey pocketDS)
  registerObjectToLocation bedroomGID pocketGID (ObjectiveKey pocketOB)

  registerSpatial chairGID (Supports (Data.Set.singleton robeGID))
  registerSpatial chairGID (SupportedBy floorGID)
  registerSpatial robeGID (SupportedBy chairGID)
  registerSpatial pocketGID (ContainedIn robeGID)

  -- Create all effects first
  openEyesLookChangeEffectPlayer <- makeEffect isaLook lookFGID
  openEyesLookChangeEffectFloor <- makeEffect dsaLook lookAtFloorFGID
  openeEyesLooKChangeEffectChair <- makeEffect dsaLook lookAtChairGID
  openEyesLookChangeEffectRobe <- makeEffect dsaLook lookAtRobeFGID
  openEyesOpenPocketChangesForPlayer <- makeEffect openSA accessContainerFGID
  robeOpenEyesLookChangesGetRobeForPlayer <- makeEffect getRobeAVP playerGetFGID
  robeOpenEyesLookChangesGetRobePhraseForRobe <- makeEffect getRobeAVP getRobeFGID
  robeOpenEyesLookChangesGetRobeForRobe <- makeEffect get getRobeFGID
  
  -- Create narration effects
  pitchBlackNarr <- pitchBlackNarration
  openEyesNarr <- openEyesSuccessNarration
  eyesClosedFloorNarr <- eyesClosedFloorNarration
  eyesClosedChairNarr <- eyesClosedChairNarration
  eyesClosedRobeNarr <- eyesClosedRobeNarration
  getRobeDeniedNarr <- getRobeDeniedNarration
  pickUpNarr <- pickUpNarration
  pocketOutOfReachNarr <- pocketOutOfReachNarration

  -- Build composed effect computation
  buildEffects $
    -- Narration effects for actions attempted with eyes closed
    buildEffect (ImplicitStimulusActionKey pitchBlackGID) bedroomGID pitchBlackNarr `alongside`
    buildEffect (DirectionalStimulusActionKey notEvenFloorFGID) floorGID eyesClosedFloorNarr `alongside`
    buildEffect (DirectionalStimulusActionKey whatChairFGID) chairGID eyesClosedChairNarr `alongside`
    buildEffect (DirectionalStimulusActionKey notEvenRobeFGID) robeGID eyesClosedRobeNarr `alongside`
    buildEffect (AcquisitionalActionKey getRobeDeniedGID) robeGID getRobeDeniedNarr `alongside`
    buildEffect (ContainerAccessActionKey openPocketNoReachGID) pocketGID pocketOutOfReachNarr `andThen`
    
    -- Effects that happen when eyes are opened
    buildEffect (SomaticAccessActionKey openEyesGID) bedroomGID openEyesLookChangeEffectPlayer `alongside`
    buildEffect (SomaticAccessActionKey openEyesGID) floorGID openEyesLookChangeEffectFloor `alongside`
    buildEffect (SomaticAccessActionKey openEyesGID) chairGID openeEyesLooKChangeEffectChair `alongside`
    buildEffect (SomaticAccessActionKey openEyesGID) robeGID openEyesLookChangeEffectRobe `andThen`

    buildEffect (SomaticAccessActionKey openEyesGID) (PlayerKeyObject pocketGID) openEyesOpenPocketChangesForPlayer `andThen`
    buildEffect (SomaticAccessActionKey openEyesGID) (PlayerKeyObject robeGID) robeOpenEyesLookChangesGetRobeForPlayer `andThen`
    buildEffect (SomaticAccessActionKey openEyesGID) robeGID robeOpenEyesLookChangesGetRobePhraseForRobe `andThen`
    buildEffect (SomaticAccessActionKey openEyesGID) robeGID robeOpenEyesLookChangesGetRobeForRobe `andThen`
    
    -- Perception narration triggered by opening eyes
    buildEffect (SomaticAccessActionKey openEyesGID) PlayerKey SomaticNarrationEffect `andThen`
    
    -- Success narrations - triggered when actions succeed  
    buildEffect (AcquisitionalActionKey getRobeFGID) robeGID pickUpNarr

  finalizeGameState

-- Helper functions using HasBehavior - much cleaner!
buildLocation :: GID ImplicitStimulusActionF -> SashaDSL Location
buildLocation implicitLookResponseGID =
  defaultLocation &
    (withTitle "bedroom in bed" >=>
    withBehavior (makeBehavior isaLook implicitLookResponseGID))

floorObj :: GID DirectionalStimulusActionF -> SashaDSL Object
floorObj lookGID = defaultObject &
  (withShortName "<OBJ-001>floor" >=>
  withDescription "The bedroom floor" >=>
  withDescriptives [SimpleNounPhrase floorDS] >=>
  withBehavior (makeBehavior look lookGID))

chairObj :: GID DirectionalStimulusActionF -> GID AcquisitionActionF -> SashaDSL Object
chairObj lookGID getGID = defaultObject &
  (withShortName "<OBJ-002>chair" >=>
  withDescription "A simple wooden chair" >=>
  withDescriptives [SimpleNounPhrase chairDS] >=>
  withBehavior (makeBehavior look lookGID) >=>
  withBehavior (makeBehavior get getGID) >=>
  withBehavior (makeBehavior getRobeAVP getGID))

robeObj :: GID DirectionalStimulusActionF -> GID AcquisitionActionF -> SashaDSL Object
robeObj lookGID getGID = defaultObject &
  (withShortName "<OBJ-003>comfortable robe" >=>
  withDescription "The robe is draped on the chair" >=>
  withDescriptives [SimpleNounPhrase robeDS] >=>
  withBehavior (makeBehavior look lookGID) >=>
  withBehavior (makeBehavior get getGID) >=>
  withBehavior (makeBehavior getRobeAVP getGID))

pocketObj :: GID DirectionalStimulusActionF -> GID ContainerAccessActionF -> SashaDSL Object
pocketObj lookGID openGID = defaultObject &
  (withShortName "<OBJ-004>pocket" >=>
  withDescription "A pocket sewn into the robe" >=>
  withDescriptives [SimpleNounPhrase pocketDS] >=>
  withBehavior (makeBehavior look lookGID) >=>
  withBehavior (makeBehavior openSA openGID))

buildBedroomPlayer :: GID Location
                   -> GID ImplicitStimulusActionF
                   -> GID ImplicitStimulusActionF
                   -> GID SomaticAccessActionF
                   -> GID DirectionalStimulusActionF
                   -> GID AcquisitionActionF
                   -> GID ContainerAccessActionF
                   -> SashaDSL Player
buildBedroomPlayer bedroomGID implicitLookResponseGID inventoryFGID openEyesGID directLookResponseGID getRobeFGID containerAccessDeniedF =
  withPlayerLocation defaultPlayer bedroomGID >>=
  withBehavior (makeBehavior isaLook implicitLookResponseGID) >>=
  withBehavior (makeBehavior inventory inventoryFGID) >>=
  withBehavior (makeBehavior look directLookResponseGID) >>=
  withBehavior (makeBehavior saOpen openEyesGID) >>=
  withBehavior (makeBehavior getRobeAVP getRobeFGID) >>=
  withBehavior (makeBehavior get getRobeFGID) >>=
  withBehavior (makeBehavior openSA containerAccessDeniedF) >>=
  withBehavior (makeBehavior openPocketCVP containerAccessDeniedF)
