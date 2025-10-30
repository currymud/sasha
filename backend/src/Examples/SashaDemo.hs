{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Examples.SashaDemo where

import           Control.Monad                                           ((>=>))
import qualified Data.Set

-- Core imports
import           EDSL.Actions.HasAction                                  (declareAction)
import           EDSL.Effects.EffectAlgebra                              (alongside,
                                                                          buildEffect,
                                                                          buildEffects)
import           EDSL.Effects.HasBehavior                                (HasBehavior (withBehavior),
                                                                          MakeBehavior (makeBehavior),
                                                                          makeAgentBehavior,
                                                                          makeAgentContainerAccessBehavior,
                                                                          makeAgentContainerAccessPhraseBehavior,
                                                                          makeAgentDSBehavior,
                                                                          makeAgentISBehavior,
                                                                          makeAgentPhraseBehavior,
                                                                          makeAgentPosturalBehavior,
                                                                          makeContainerBehavior,
                                                                          makeContainerPhraseBehavior,
                                                                          makeLocationCDSBehavior,
                                                                          makeLocationContainerAccessPhraseBehavior,
                                                                          makeLocationDSBehavior,
                                                                          makeLocationISBehavior,
                                                                          makeObjectBehavior,
                                                                          makeObjectContainerAccessBehavior,
                                                                          makeObjectContainerAccessPhraseBehavior,
                                                                          makeObjectDSBehavior,
                                                                          makeObjectPhraseBehavior)
import           EDSL.Effects.HasEffect                                  (HasEffect (linkEffect),
                                                                          makeAgentCDSEffect,
                                                                          makeAgentDSEffect,
                                                                          makeAgentISEffect,
                                                                          makeAgentPhraseEffect,
                                                                          makeContainerCDSEffect,
                                                                          makeObjectDSEffect,
                                                                          makeObjectEffect,
                                                                          makeObjectPhraseEffect)
import           Examples.Defaults                                       (defaultLocation,
                                                                          defaultObject,
                                                                          defaultPlayer)
import           Model.Core                                              (ActionEffectKey (..),
                                                                          ActionGID (AgentContainerAccessActionGID),
                                                                          ActionManagementOperation (AddAgentContainerAccessVerbPhrase),
                                                                          AgentAcquisitionActionF,
                                                                          AgentContainerAccessActionF,
                                                                          AgentDirectionalStimulusActionF (AgentCanLookAtF),
                                                                          AgentImplicitStimulusActionF,
                                                                          AgentPosturalActionF,
                                                                          ContainerAcquisitionActionF,
                                                                          Effect (..),
                                                                          FieldUpdateOperation (ObjectDescription),
                                                                          GameState,
                                                                          Location,
                                                                          LocationContainerAccessActionF,
                                                                          LocationDirectionalStimulusActionF (LocationCannotBeSeenF),
                                                                          LocationDirectionalStimulusContainerActionF,
                                                                          LocationImplicitStimulusActionF,
                                                                          NarrationComputation (..),
                                                                          Object,
                                                                          ObjectAcquisitionActionF,
                                                                          ObjectContainerAccessActionF,
                                                                          ObjectDirectionalStimulusActionF (ObjectCannotBeSeenF'),
                                                                          Player,
                                                                          PlayerKey (..),
                                                                          SomaticAccessActionF,
                                                                          SpatialRelationship (..))
import           Model.EDSL.SashaLambdaDSL                               (SashaLambdaDSL,
                                                                          createObjectContainerAccessSimpleVerbEffect,
                                                                          createObjectContainerAccessVerbPhraseEffect,
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
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (dsaLook)
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb    (inventory,
                                                                          isaLook)
import           Grammar.Parser.Partitions.Verbs.PosturalVerbs           (stand)
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
import           ConstraintRefinement.Actions.Locations.Look             (allowLookF,
                                                                          locationAllowLookAtF,
                                                                          locationAllowLookInF)
import           ConstraintRefinement.Actions.Locations.Open             (openLocationF)
import           ConstraintRefinement.Actions.Objects.Look               (containerCanBeSeenInF,
                                                                          containerCannotBeSeenInF,
                                                                          objectCanBeSeenF,
                                                                          objectCannotBeSeenF)
import           ConstraintRefinement.Actions.Objects.Open               (openObjectContainerF)
import           ConstraintRefinement.Actions.Player.Get                 (getF)
import           ConstraintRefinement.Actions.Player.Look                (agentCannotLookF,
                                                                          agentLookAtFailF,
                                                                          agentLookF,
                                                                          lookAtF,
                                                                          lookInF)
import           ConstraintRefinement.Actions.Player.Open                (openContainerDeniedF,
                                                                          openContainerF,
                                                                          openEyes)
import           ConstraintRefinement.Actions.Player.Stand               (standF)
import           ConstraintRefinement.Actions.RoleBased.Constructors     (agentCannotAcquireF,
                                                                          containerLosesObjectF,
                                                                          objectCollectedF,
                                                                          objectNotCollectableF)
import           Data.Function                                           ((&))
import           Data.Text                                               (Text)
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
  eyesClosedFGID <- declareAction agentCannotLookF
  cannotStandFGID <- declareAction standF
  canStandFGID <- declareAction standF
  locationLitFGID <- declareAction allowLookF
  -- Location directional stimulus actions
  locationCanBeSeenGID <- declareAction  locationAllowLookAtF
  locationCanBeSeenInFGID <- declareAction locationAllowLookInF
  locationOpenContainerFGID <- declareAction openLocationF
  lookAtFloorFGID <- declareAction objectCanBeSeenF
  notEvenFloorFGID <- declareAction objectCannotBeSeenF
  lookAtChairGID <- declareAction objectCanBeSeenF
  whatChairFGID <- declareAction objectCannotBeSeenF

  -- Use role-based container action for chair losing object
  getFromChairGID <- declareAction (containerLosesObjectF chairGID)
  lookAtRobeFGID <- declareAction objectCanBeSeenF
  notEvenRobeFGID <- declareAction objectCannotBeSeenF
  getRobeDeniedGID <- declareAction objectNotCollectableF

  -- Use role-based object action for robe being collected
  getRobeFGID <- declareAction (objectCollectedF robeGID)
  lookAtPocketGID <- declareAction objectCanBeSeenF
  lookAtPillGID <- declareAction objectCanBeSeenF
  lookAtPillDeniedGID <- declareAction objectCannotBeSeenF
  lookInPocketGID <- declareAction containerCanBeSeenInF
  openPocketNoReachGID <- declareAction openObjectContainerF

  openEyesGID <- declareAction openEyes
  -- Use role-based agent action for player acquisition denial
  getDeniedFGID <- declareAction agentCannotAcquireF
  lookAtDeniedFGID <- declareAction agentLookAtFailF
  lookInPocketDeniedFGID <- declareAction containerCannotBeSeenInF
  -- Use role-based agent action for player get coordination
  playerGetFGID <- declareAction getF
  inventoryFGID <- declareAction agentLookF
  playerLookFGID <- declareAction agentLookF
  playerLookAtFGID <- declareAction lookAtF
  playerLookInFGID <- declareAction lookInF

  containerAccessDeniedFGID <- declareAction openContainerDeniedF
  containerAccessAllowedFGID <- declareAction openContainerF
  accessContainerFGID <- declareAction openObjectContainerF
  openContainerFGID <- declareAction openObjectContainerF
  registerLocation bedroomGID (buildLocation locationLitFGID locationCanBeSeenGID locationCanBeSeenInFGID locationOpenContainerFGID)
  registerObject floorGID (floorObj notEvenFloorFGID)
  registerObject chairGID (chairObj whatChairFGID getFromChairGID)
  registerObject robeGID (robeObj notEvenRobeFGID getRobeDeniedGID)
  registerObject pocketGID (pocketObj lookAtPocketGID openPocketNoReachGID)
  registerObject pillGID (pillObj lookAtPillDeniedGID)
  player <- buildBedroomPlayer bedroomGID eyesClosedFGID inventoryFGID openEyesGID
                   lookAtDeniedFGID getDeniedFGID containerAccessDeniedFGID cannotStandFGID
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
  registerObjectToLocation bedroomGID pillGID (DirectionalStimulusKey pillDS)
  -- Floor is the anchor object (not supported by anything)
  registerSpatial floorGID (Supports (Data.Set.singleton chairGID))
  registerSpatial chairGID (Supports (Data.Set.singleton robeGID))
  registerSpatial chairGID (SupportedBy floorGID)
  registerSpatial robeGID (SupportedBy chairGID)
  registerSpatial pocketGID (SupportedBy robeGID)
  registerSpatial pillGID (ContainedIn pocketGID)
  registerSpatial pocketGID (Contains (Data.Set.singleton pillGID))
  -- Create all effects first
  openEyesLookChangeEffectPlayer <- makeAgentISEffect isaLook playerLookFGID
  openEyesLookAtChangeEffectPlayer <- makeAgentDSEffect dsaLook playerLookAtFGID
  openEyesLookInChangeEffectPlayer <- makeAgentCDSEffect dsaLook playerLookInFGID
  pillVisibleAfterOpenEffect <- makeObjectDSEffect dsaLook lookAtPillGID
  pocketOpenForLookIn <- makeContainerCDSEffect dsaLook lookInPocketGID
  openEyesLookChangeEffectFloor <- makeObjectDSEffect dsaLook lookAtFloorFGID
  openeEyesLooKChangeEffectChair <- makeObjectDSEffect dsaLook lookAtChairGID
  openEyesLookChangeEffectRobe <- makeObjectDSEffect dsaLook lookAtRobeFGID
  openEyesOpenPocketChangesForPlayer <- createObjectContainerAccessSimpleVerbEffect openSA accessContainerFGID
  robeOpenEyesLookChangesGetRobeForPlayer <- makeAgentPhraseEffect getRobeAVP playerGetFGID
  robeOpenEyesLookChangesGetRobePhraseForRobe <- makeObjectPhraseEffect getRobeAVP getRobeFGID
  robeOpenEyesLookChangesGetRobeForRobe <- makeObjectEffect get getRobeFGID
  pocketOpenGetRobe <- createObjectContainerAccessVerbPhraseEffect openPocketCVP openContainerFGID
  pocketLookInGetRobe <- makeAgentCDSEffect dsaLook playerLookInFGID
  playerOpenPocketAfterRobe <- createObjectContainerAccessVerbPhraseEffect openPocketCVP accessContainerFGID
  pocketClosed <- makeContainerCDSEffect dsaLook lookInPocketDeniedFGID
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
    buildEffect (ObjectAcquisitionalActionKey getRobeFGID) robeGID (FieldUpdateEffect (ObjectDescription robeGID gotRobeDescription)) `alongside`
    buildEffect (ObjectAcquisitionalActionKey getRobeFGID) pocketGID (FieldUpdateEffect (ObjectDescription pocketGID robePocketDescription)) `alongside`
    buildEffect (ObjectAcquisitionalActionKey getRobeFGID) pocketGID pocketOpenGetRobe `alongside`
    buildEffect (ObjectAcquisitionalActionKey getRobeFGID) (PlayerKeyObject pocketGID) playerOpenPocketAfterRobe `alongside`
    buildEffect (ObjectAcquisitionalActionKey getRobeFGID) (PlayerKeyObject pocketGID) pocketLookInGetRobe `alongside`
    buildEffect (ObjectAcquisitionalActionKey getRobeFGID) pocketGID pocketClosed `alongside`
    buildEffect (ObjectAcquisitionalActionKey getRobeFGID) (PlayerKeyLocation bedroomGID) (ActionManagementEffect (AddAgentContainerAccessVerbPhrase openPocketCVP containerAccessAllowedFGID) (AgentContainerAccessActionGID containerAccessAllowedFGID)) `alongside`
    buildEffect (ObjectContainerAccessActionKey openContainerFGID) pocketGID (FieldUpdateEffect (ObjectDescription pocketGID openPocketDescription)) `alongside`
    buildEffect (ObjectContainerAccessActionKey openContainerFGID) pillGID (FieldUpdateEffect (ObjectDescription pillGID "A small round pill, now within your reach.")) `alongside`
    buildEffect (ObjectContainerAccessActionKey openContainerFGID) pillGID pillVisibleAfterOpenEffect `alongside`
    buildEffect (ObjectContainerAccessActionKey openContainerFGID) pocketGID pocketOpenForLookIn
  -- Register narration effects for actions
  linkEffect (SomaticAccessActionKey openEyesGID) (PlayerKeyLocation bedroomGID)
    (NarrationEffect (StaticNarration "You open your eyes, and the world comes into focus."))
  linkEffect (SomaticAccessActionKey openEyesGID) (PlayerKeyLocation bedroomGID)
    (NarrationEffect LookNarration)
  linkEffect (AgentImplicitStimulusActionKey eyesClosedFGID) (PlayerKeyLocation bedroomGID)
    (NarrationEffect (StaticNarration closedEyes))
  -- Add narration for agent's directional stimulus when eyes closed
  linkEffect (AgentDirectionalStimulusActionKey lookAtDeniedFGID) (PlayerKeyLocation bedroomGID)
    (NarrationEffect (StaticNarration closedEyes))
  -- Inventory narration
  linkEffect (AgentImplicitStimulusActionKey inventoryFGID) (PlayerKeyLocation bedroomGID)
    (NarrationEffect InventoryNarration)

  linkEffect (AgentImplicitStimulusActionKey playerLookFGID) (PlayerKeyLocation bedroomGID)
    (NarrationEffect LookNarration)
  linkEffect (ContainerDirectionalStimulusContainerActionKey lookInPocketDeniedFGID) pocketGID
    (NarrationEffect (StaticNarration closedPocket))
--lookInPocketDeniedFGID
  -- LookAt narration for objects
  linkEffect (ObjectDirectionalStimulusActionKey lookAtFloorFGID) floorGID
    (NarrationEffect (LookAtNarration floorGID))

  linkEffect (ObjectDirectionalStimulusActionKey lookAtChairGID) chairGID
    (NarrationEffect (LookAtNarration chairGID))

  linkEffect (ObjectDirectionalStimulusActionKey lookAtRobeFGID) robeGID
    (NarrationEffect (LookAtNarration robeGID))

  linkEffect (ObjectDirectionalStimulusActionKey lookAtPocketGID) pocketGID
    (NarrationEffect (LookAtNarration pocketGID))

  linkEffect (ObjectDirectionalStimulusActionKey notEvenRobeFGID) robeGID
    (NarrationEffect (StaticNarration closedEyes))
  -- Static narration for player's get action
  linkEffect (AgentAcquisitionalActionKey playerGetFGID) (PlayerKeyObject robeGID)
    (NarrationEffect (StaticNarration "You pick it up."))

  linkEffect (AgentContainerAccessActionKey containerAccessDeniedFGID) pocketGID
    (NarrationEffect (StaticNarration "You cannot reach the pocket."))

  linkEffect (AgentContainerAccessActionKey containerAccessAllowedFGID) pocketGID
    (NarrationEffect (StaticNarration "You open the pocket."))

  linkEffect (ObjectContainerAccessActionKey openContainerFGID) pocketGID
    (NarrationEffect (LookInNarration pocketGID))

  linkEffect (ObjectContainerAccessActionKey openContainerFGID) pocketGID
    (NarrationEffect (LookAtNarration pocketGID))
  linkEffect (AgentAcquisitionalActionKey getDeniedFGID) (PlayerKeyObject robeGID)
    (NarrationEffect (StaticNarration getDenied))
  linkEffect (ContainerDirectionalStimulusContainerActionKey lookInPocketGID) pocketGID
    (NarrationEffect (LookInNarration pocketGID))

  linkEffect (ObjectDirectionalStimulusActionKey lookAtPillDeniedGID) pillGID
    (NarrationEffect (StaticNarration "The pill is in the closed pocket - you can't see it clearly."))

  linkEffect (ObjectDirectionalStimulusActionKey lookAtPillGID) pillGID
    (NarrationEffect (LookAtNarration pillGID))

  linkEffect (AgentPosturalActionKey cannotStandFGID) (PlayerKeyLocation bedroomGID)
    (NarrationEffect (StaticNarration "You try to stand up, but you feel too dizzy to do so."))
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

    closedPocket :: Text
    closedPocket = "The pocket is closed."
buildLocation :: GID LocationImplicitStimulusActionF
                   -> GID LocationDirectionalStimulusActionF
                    -> GID LocationDirectionalStimulusContainerActionF
                   -> GID LocationContainerAccessActionF
                   -> SashaLambdaDSL Location
buildLocation implicitLookResponseGID locationLookFGID locationLookInFGID locationOpenContainerFGID =
  defaultLocation &
    (withTitle "bedroom in bed" >=>
    withBehavior (makeLocationISBehavior isaLook implicitLookResponseGID) >=>
    withBehavior (makeLocationDSBehavior dsaLook locationLookFGID) >=>
    withBehavior (makeLocationCDSBehavior dsaLook locationLookInFGID) >=>
    withBehavior (makeLocationContainerAccessPhraseBehavior openPocketCVP locationOpenContainerFGID))

floorObj :: GID ObjectDirectionalStimulusActionF -> SashaLambdaDSL Object
floorObj lookGID = defaultObject &
  (withShortName "<OBJ-001>floor" >=>
  withDescription "The bedroom floor" >=>
  withDescriptives [SimpleNounPhrase floorDS] >=>
  withBehavior (makeObjectDSBehavior dsaLook lookGID))

chairObj :: GID ObjectDirectionalStimulusActionF -> GID ContainerAcquisitionActionF -> SashaLambdaDSL Object
chairObj lookGID getGID = defaultObject &
  (withShortName "<OBJ-002>chair" >=>
  withDescription "A simple wooden chair" >=>
  withDescriptives [SimpleNounPhrase chairDS] >=>
  withBehavior (makeObjectDSBehavior dsaLook lookGID) >=>
  withBehavior (makeContainerBehavior get getGID) >=>
  withBehavior (makeContainerPhraseBehavior getRobeAVP getGID))

robeObj :: GID ObjectDirectionalStimulusActionF -> GID ObjectAcquisitionActionF -> SashaLambdaDSL Object
robeObj lookGID getGID = defaultObject &
  (withShortName "<OBJ-003>comfortable robe" >=>
  withDescription "The robe is draped on the chair" >=>
  withDescriptives [SimpleNounPhrase robeDS] >=>
  withBehavior (makeObjectDSBehavior dsaLook lookGID) >=>
  withBehavior (makeObjectBehavior get getGID) >=>
  withBehavior (makeObjectPhraseBehavior getRobeAVP getGID))

pillObj :: GID ObjectDirectionalStimulusActionF -> SashaLambdaDSL Object
pillObj lookFailGIDF = defaultObject &
  (withShortName "<OBJ-005>pill" >=>
  withDescription "A small round pill" >=>
  withDescriptives [SimpleNounPhrase pillDS] >=>
  withBehavior (makeObjectDSBehavior dsaLook lookFailGIDF))


pocketObj :: GID ObjectDirectionalStimulusActionF -> GID ObjectContainerAccessActionF -> SashaLambdaDSL Object
pocketObj lookGID openGID = defaultObject &
  (withShortName "<OBJ-004>pocket" >=>
  withDescription "A pocket sewn into the robe" >=>
  withDescriptives [SimpleNounPhrase pocketDS] >=>
  withBehavior (makeObjectDSBehavior dsaLook lookGID) >=>
  withBehavior (makeObjectContainerAccessBehavior openSA openGID) >=>
  withBehavior (makeObjectContainerAccessPhraseBehavior openPocketCVP openGID))

buildBedroomPlayer :: GID Location
                   -> GID AgentImplicitStimulusActionF
                   -> GID AgentImplicitStimulusActionF
                   -> GID SomaticAccessActionF
                   -> GID AgentDirectionalStimulusActionF
                   -> GID AgentAcquisitionActionF
                   -> GID AgentContainerAccessActionF
                   -> GID AgentPosturalActionF
                   -> SashaLambdaDSL Player
buildBedroomPlayer bedroomGID implicitLookResponseGID inventoryFGID openEyesGID directLookResponseGID getRobeFGID containerAccessDeniedF cannotStandFGID =
  withPlayerLocation defaultPlayer bedroomGID >>=
  withBehavior (makeAgentISBehavior isaLook implicitLookResponseGID) >>=
  withBehavior (makeAgentISBehavior inventory inventoryFGID) >>=
  withBehavior (makeAgentDSBehavior dsaLook directLookResponseGID) >>=
  withBehavior (makeBehavior saOpen openEyesGID) >>=
  withBehavior (makeAgentPhraseBehavior getRobeAVP getRobeFGID) >>=
  withBehavior (makeAgentBehavior get getRobeFGID) >>=
  withBehavior (makeAgentContainerAccessBehavior openSA containerAccessDeniedF) >>=
  withBehavior (makeAgentContainerAccessPhraseBehavior openPocketCVP containerAccessDeniedF) >>=
  withBehavior (makeAgentPosturalBehavior stand cannotStandFGID)
