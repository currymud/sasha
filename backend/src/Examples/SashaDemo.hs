{-# LANGUAGE OverloadedStrings #-}

module Examples.SashaDemo where

import           Control.Monad                                           ((>=>))
import qualified Data.Set

-- Core imports
import           EDSL.Actions.HasAction                                  (declareAction)
import           EDSL.Effects.EffectAlgebra                              (alongside,
                                                                          buildEffect,
                                                                          buildEffects)
import           EDSL.Effects.HasBehavior                                (HasBehavior (withBehavior),
                                                                          MakeBehavior (makeBehavior))
import           EDSL.Effects.HasEffect                                  (HasEffect (linkEffect),
                                                                          MakeEffect (makeEffect))
import           Examples.Defaults                                       (defaultLocation,
                                                                          defaultObject,
                                                                          defaultPlayer)
import           Model.Core                                              (AcquisitionActionF,
                                                                          AgentAcquisitionActionF,
                                                                          ObjectAcquisitionActionF,
                                                                          ContainerAcquisitionActionF,
                                                                          ActionEffectKey (..),
                                                                          ContainerAccessActionF,
                                                                          DirectionalStimulusActionF,
                                                                          Effect (..),
                                                                          FieldUpdateOperation (ObjectDescription),
                                                                          GameState,
                                                                          ImplicitStimulusActionF,
                                                                          Location,
                                                                          NarrationComputation (..),
                                                                          Object,
                                                                          Player,
                                                                          PlayerKey (..),
                                                                          SomaticAccessActionF,
                                                                          SpatialRelationship (..))
import           Model.EDSL.SashaLambdaDSL                               (SashaLambdaDSL,
                                                                          declareLocationGID,
                                                                          declareObjectGID,
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

-- All the noun/verb imports from original
import           Grammar.Parser.Partitions.Nouns.Containers              (pocketCT)
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus     (bedroomDS,
                                                                          chairDS,
                                                                          floorDS,
                                                                          pillDS,
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
import           Model.Parser.Composites.Nouns                           (ConsumableNounPhrase (ConsumableNounPhrase),
                                                                          ContainerPhrase (ContainerPhrase),
                                                                          NounPhrase (SimpleNounPhrase),
                                                                          ObjectPhrase (ObjectPhrase))
import           Model.Parser.Composites.Verbs                           (AcquisitionVerbPhrase (SimpleAcquisitionVerbPhrase),
                                                                          ConsumptionVerbPhrase (ConsumptionVerbPhrase),
                                                                          ContainerAccessVerbPhrase (SimpleAccessContainerVerbPhrase))
import           Model.Parser.GCase                                      (NounKey (ContainerKey, DirectionalStimulusKey, ObjectiveKey, SurfaceKey))

-- Action functions from original
import           ConstraintRefinement.Actions.Locations.Look             (lookF,
                                                                          pitchBlackF)
import           ConstraintRefinement.Actions.Objects.Get.Constructors   (getFromSupportF,
                                                                          getObjectF,
                                                                          objectNotGettableF)
import           ConstraintRefinement.Actions.Objects.Look               (cannotBeSeenF)
import           ConstraintRefinement.Actions.Objects.Open               (openContainerF)
import           ConstraintRefinement.Actions.Player.Get                 (getDeniedF,
                                                                          getF)
import           ConstraintRefinement.Actions.Player.Inventory           (defaultInventoryLookF)
import           ConstraintRefinement.Actions.Player.Look                (dsvActionEnabled,
                                                                          isvActionEnabled,
                                                                          lookAtF)
import           ConstraintRefinement.Actions.Player.Open                (openDeniedF,
                                                                          openEyes,
                                                                          openF)
import           ConstraintRefinement.Actions.RoleBased.Constructors  (agentGetF,
                                                                          objectCollectedF,
                                                                          containerLosesObjectF,
                                                                          agentCannotAcquireF,
                                                                          objectNotCollectableF,
                                                                          containerCannotReleaseF)
import           ConstraintRefinement.Actions.RoleBased.Conversions   (toAcquisitionActionF,
                                                                          objectToAcquisitionActionF,
                                                                          containerToAcquisitionActionF)
import           Data.Function                                           ((&))
import           Data.Text                                               (Text)
import           GHC.TypeError                                           (ErrorMessage (Text))
import           Grammar.Parser.Partitions.Nouns.Consumables             (pillCS)
import           Grammar.Parser.Partitions.Verbs.ConsumptionVerbs        (takeCV)

-- Verb phrases from original
getRobeAVP :: AcquisitionVerbPhrase
getRobeAVP = SimpleAcquisitionVerbPhrase get (ObjectPhrase (SimpleNounPhrase robeOB))

openPocketCVP :: ContainerAccessVerbPhrase
openPocketCVP = SimpleAccessContainerVerbPhrase openSA (ContainerPhrase (SimpleNounPhrase pocketCT))

takePillCVP :: ConsumptionVerbPhrase
takePillCVP = ConsumptionVerbPhrase takeCV (ConsumableNounPhrase (SimpleNounPhrase pillCS))
-- | Main demo function with HasBehavior and HasEffect - same signature as original
sashaBedroomDemo :: SashaLambdaDSL GameState
sashaBedroomDemo = do
  bedroomGID <- declareLocationGID (SimpleNounPhrase bedroomDS)
  floorGID <- declareObjectGID (SimpleNounPhrase floorDS)
  chairGID <- declareObjectGID (SimpleNounPhrase chairDS)
  robeGID <- declareObjectGID (SimpleNounPhrase robeDS)
  pocketGID <- declareObjectGID (SimpleNounPhrase pocketDS)
  pillGID <- declareObjectGID (SimpleNounPhrase pillDS)

  pitchBlackFGID <- declareAction pitchBlackF
  lookAtFloorFGID <- declareAction lookAtF
  notEvenFloorFGID <- declareAction cannotBeSeenF
  lookAtChairGID <- declareAction lookAtF
  whatChairFGID <- declareAction cannotBeSeenF

  -- Use role-based container action for chair losing object
  getFromChairGID <- declareAction (containerToAcquisitionActionF (containerLosesObjectF chairGID))
  lookAtRobeFGID <- declareAction lookAtF
  notEvenRobeFGID <- declareAction cannotBeSeenF
  getRobeDeniedGID <- declareAction (objectToAcquisitionActionF objectNotCollectableF)

  -- Use role-based object action for robe being collected  
  getRobeFGID <- declareAction (objectToAcquisitionActionF (objectCollectedF robeGID))
  lookAtPocketGID <- declareAction lookAtF
  openPocketNoReachGID <- declareAction openContainerF

  openEyesGID <- declareAction openEyes
  -- Use role-based agent action for player acquisition denial  
  getDeniedFGID <- declareAction (toAcquisitionActionF agentCannotAcquireF)
  -- Use role-based agent action for player get coordination
  playerGetFGID <- declareAction (toAcquisitionActionF agentGetF)
  lookFGID <- declareAction lookF
  inventoryFGID <- declareAction defaultInventoryLookF
  dsvEnabledLookGID <- declareAction dsvActionEnabled
  containerAccessDeniedFGID <- declareAction openDeniedF
  accessContainerFGID <- declareAction openF
  openContainerFGID <- declareAction openContainerF
  registerLocation bedroomGID (buildLocation pitchBlackFGID)
  registerObject floorGID (floorObj notEvenFloorFGID)
  registerObject chairGID (chairObj whatChairFGID getFromChairGID)
  registerObject robeGID (robeObj notEvenRobeFGID getRobeDeniedGID)
  registerObject pocketGID (pocketObj lookAtPocketGID openPocketNoReachGID)

  player <- buildBedroomPlayer bedroomGID pitchBlackFGID inventoryFGID openEyesGID
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
  registerObjectToLocation bedroomGID pocketGID (ContainerKey pocketCT)

  -- Floor is the anchor object (not supported by anything)
  registerSpatial floorGID (Supports (Data.Set.singleton chairGID))
  registerSpatial chairGID (Supports (Data.Set.singleton robeGID))
  registerSpatial chairGID (SupportedBy floorGID)
  registerSpatial robeGID (SupportedBy chairGID)
  registerSpatial pocketGID (SupportedBy robeGID)

  -- Create all effects first
  openEyesLookChangeEffectPlayer <- makeEffect isaLook lookFGID
  openEyesLookAtChangeEffectPlayer <- makeEffect dsaLook dsvEnabledLookGID
  openEyesLookChangeEffectFloor <- makeEffect dsaLook lookAtFloorFGID
  openeEyesLooKChangeEffectChair <- makeEffect dsaLook lookAtChairGID
  openEyesLookChangeEffectRobe <- makeEffect dsaLook lookAtRobeFGID
  openEyesOpenPocketChangesForPlayer <- makeEffect openSA accessContainerFGID
  robeOpenEyesLookChangesGetRobeForPlayer <- makeEffect getRobeAVP playerGetFGID
  robeOpenEyesLookChangesGetRobePhraseForRobe <- makeEffect getRobeAVP getRobeFGID
  robeOpenEyesLookChangesGetRobeForRobe <- makeEffect get getRobeFGID
  pocketOpenGetRobe <- makeEffect openPocketCVP openContainerFGID
  playerOpenPocketAfterRobe <- makeEffect openPocketCVP accessContainerFGID
  -- Build composed effect computation
  buildEffects $
    buildEffect (SomaticAccessActionKey openEyesGID) (PlayerKeyObject robeGID) openEyesLookAtChangeEffectPlayer `alongside`
    buildEffect (SomaticAccessActionKey openEyesGID) (PlayerKeyLocation bedroomGID) openEyesLookChangeEffectPlayer `alongside`
    buildEffect (SomaticAccessActionKey openEyesGID) floorGID openEyesLookChangeEffectFloor `alongside`
    buildEffect (SomaticAccessActionKey openEyesGID) chairGID openeEyesLooKChangeEffectChair `alongside`
    buildEffect (SomaticAccessActionKey openEyesGID) robeGID openEyesLookChangeEffectRobe `alongside`
    buildEffect (SomaticAccessActionKey openEyesGID) (PlayerKeyObject pocketGID) openEyesOpenPocketChangesForPlayer `alongside`
    buildEffect (SomaticAccessActionKey openEyesGID) (PlayerKeyObject robeGID) robeOpenEyesLookChangesGetRobeForPlayer `alongside`
    buildEffect (SomaticAccessActionKey openEyesGID) robeGID robeOpenEyesLookChangesGetRobePhraseForRobe `alongside`
    buildEffect (SomaticAccessActionKey openEyesGID) robeGID robeOpenEyesLookChangesGetRobeForRobe `alongside`
    buildEffect (AcquisitionalActionKey getRobeFGID) robeGID (FieldUpdateEffect (ObjectDescription robeGID gotRobeDescription)) `alongside`
    buildEffect (AcquisitionalActionKey getRobeFGID) pocketGID (FieldUpdateEffect (ObjectDescription pocketGID robePocketDescription)) `alongside`
    buildEffect (AcquisitionalActionKey getRobeFGID) pocketGID pocketOpenGetRobe `alongside`
    buildEffect (AcquisitionalActionKey getRobeFGID) (PlayerKeyObject pocketGID) playerOpenPocketAfterRobe `alongside`
    buildEffect (ContainerAccessActionKey openContainerFGID) pocketGID (FieldUpdateEffect (ObjectDescription pocketGID openPocketDescription))

  -- Register narration effects for actions
  linkEffect (SomaticAccessActionKey openEyesGID) (PlayerKeyLocation bedroomGID)
    (NarrationEffect (StaticNarration "You open your eyes, and the world comes into focus."))
  linkEffect (SomaticAccessActionKey openEyesGID) (PlayerKeyLocation bedroomGID)
    (NarrationEffect LookNarration)
  linkEffect (ImplicitStimulusActionKey pitchBlackFGID) (PlayerKeyLocation bedroomGID)
    (NarrationEffect (StaticNarration closedEyes))
  -- Inventory narration
  linkEffect (ImplicitStimulusActionKey inventoryFGID) (PlayerKeyLocation bedroomGID)
    (NarrationEffect InventoryNarration)

  linkEffect (ImplicitStimulusActionKey lookFGID) (PlayerKeyLocation bedroomGID)
    (NarrationEffect LookNarration)

  -- LookAt narration for objects
  linkEffect (DirectionalStimulusActionKey lookAtFloorFGID) floorGID
    (NarrationEffect (LookAtNarration floorGID))

  linkEffect (DirectionalStimulusActionKey lookAtChairGID) chairGID
    (NarrationEffect (LookAtNarration chairGID))

  linkEffect (DirectionalStimulusActionKey lookAtRobeFGID) robeGID
    (NarrationEffect (LookAtNarration robeGID))

  linkEffect (DirectionalStimulusActionKey lookAtPocketGID) pocketGID
    (NarrationEffect (LookAtNarration pocketGID))

  linkEffect (DirectionalStimulusActionKey notEvenRobeFGID) robeGID
    (NarrationEffect (StaticNarration closedEyes))
  -- Static narration for player's get action
  linkEffect (AcquisitionalActionKey playerGetFGID) (PlayerKeyObject robeGID)
    (NarrationEffect (StaticNarration "You pick it up."))

  linkEffect (ContainerAccessActionKey openContainerFGID) pocketGID
    (NarrationEffect (LookInNarration pocketGID))

  linkEffect (ContainerAccessActionKey openContainerFGID) pocketGID
    (NarrationEffect (LookAtNarration pocketGID))
  linkEffect (AcquisitionalActionKey getDeniedFGID) (PlayerKeyObject robeGID)
    (NarrationEffect (StaticNarration getDenied))
  finalizeGameState
  where
    getDenied :: Text
    getDenied = "The difficulty of getting the robe is directly related to your eyes being closed."
    closedEyes :: Text
    closedEyes = "The inability to see a dang-doodly"
                    <> "thing is directly related to your eyes being closed."
    gotRobeDescription :: Text
    gotRobeDescription = "This robe was make for wearin'. There's something in the pocket."
    robePocketDescription :: Text
    robePocketDescription = "The pocket is velcroed shut, "
                              <> "but there's an indentation of a pill on it."
    openPocketDescription :: Text
    openPocketDescription = "The pocket is open, revealing a pill."
-- Helper functions using HasBehavior - much cleaner!
buildLocation :: GID ImplicitStimulusActionF -> SashaLambdaDSL Location
buildLocation implicitLookResponseGID =
  defaultLocation &
    (withTitle "bedroom in bed" >=>
    withBehavior (makeBehavior isaLook implicitLookResponseGID))

floorObj :: GID DirectionalStimulusActionF -> SashaLambdaDSL Object
floorObj lookGID = defaultObject &
  (withShortName "<OBJ-001>floor" >=>
  withDescription "The bedroom floor" >=>
  withDescriptives [SimpleNounPhrase floorDS] >=>
  withBehavior (makeBehavior look lookGID))

chairObj :: GID DirectionalStimulusActionF -> GID AcquisitionActionF -> SashaLambdaDSL Object
chairObj lookGID getGID = defaultObject &
  (withShortName "<OBJ-002>chair" >=>
  withDescription "A simple wooden chair" >=>
  withDescriptives [SimpleNounPhrase chairDS] >=>
  withBehavior (makeBehavior look lookGID) >=>
  withBehavior (makeBehavior get getGID) >=>
  withBehavior (makeBehavior getRobeAVP getGID))

robeObj :: GID DirectionalStimulusActionF -> GID AcquisitionActionF -> SashaLambdaDSL Object
robeObj lookGID getGID = defaultObject &
  (withShortName "<OBJ-003>comfortable robe" >=>
  withDescription "The robe is draped on the chair" >=>
  withDescriptives [SimpleNounPhrase robeDS] >=>
  withBehavior (makeBehavior look lookGID) >=>
  withBehavior (makeBehavior get getGID) >=>
  withBehavior (makeBehavior getRobeAVP getGID))

pocketObj :: GID DirectionalStimulusActionF -> GID ContainerAccessActionF -> SashaLambdaDSL Object
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
                   -> SashaLambdaDSL Player
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
