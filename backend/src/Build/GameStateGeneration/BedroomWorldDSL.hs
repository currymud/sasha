{-# LANGUAGE OverloadedStrings #-}

module Build.GameStateGeneration.BedroomWorldDSL where

import           Build.GameStateGeneration.Defaults                      (defaultLocation,
                                                                          defaultObject,
                                                                          defaultPlayer)
import qualified Data.Set                                                as Set
import           Model.GameState.GameStateDSL                            (WorldDSL,
                                                                          createAcquisitionVerbEffect,
                                                                          createAcquisitionVerbPhraseEffect,
                                                                          createConsumptionEffect,
                                                                          createDirectionalStimulusEffect,
                                                                          createImplicitStimulusEffect,
                                                                          createSomaticAccessEffect,
                                                                          declareLocationGID,
                                                                          declareObjectGID,
                                                                          displayVisibleObjects,
                                                                          finalizeGameState,
                                                                          linkActionKeyToSystemEffect,
                                                                          linkEffectToLocation,
                                                                          linkEffectToObject,
                                                                          linkEffectToPlayer,
                                                                          linkFieldEffectToObject,
                                                                          registerLocation,
                                                                          registerObject,
                                                                          registerObjectToLocation,
                                                                          registerPlayer,
                                                                          registerSpatial,
                                                                          registerSystemEffect,
                                                                          setPerceptionMap,
                                                                          updateDescription,
                                                                          withDescription,
                                                                          withDescriptives,
                                                                          withLocationBehavior,
                                                                          withObjectBehavior,
                                                                          withPlayerBehaviors,
                                                                          withPlayerLocation,
                                                                          withShortName,
                                                                          withTitle)
import           Model.GID                                               (GID (GID))
import           Model.Parser.GCase                                      (NounKey (DirectionalStimulusKey, ObjectiveKey))
import           Prelude                                                 hiding
                                                                         (take)


-- Import semantic wrappers - DirectionalStimulus versions
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus     (bedroomDS,
                                                                          chairDS,
                                                                          floorDS,
                                                                          mailDS,
                                                                          pillDS,
                                                                          pocketDS,
                                                                          robeDS,
                                                                          tableDS)
import           Grammar.Parser.Partitions.Nouns.Objectives              (chairOB,
                                                                          floorOB,
                                                                          mailOB,
                                                                          pillOB,
                                                                          robeOB,
                                                                          tableOB)

-- Import adjectives and determiners
import           Grammar.Parser.Partitions.Adjectives                    (small)
import           Grammar.Parser.Partitions.Misc                          (the)
import           Model.Parser.Composites.Nouns                           (ConsumableNounPhrase (ConsumableNounPhrase),
                                                                          DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                                          NounPhrase (DescriptiveNounPhraseDet, SimpleNounPhrase),
                                                                          ObjectPhrase (ObjectPhrase))

-- Import behavior management constructors and spatial relationships
import           Model.GameState                                         (ActionManagement (AAManagementKey, AVManagementKey, CAManagementKey, CVManagementKey, DSAManagementKey, ISAManagementKey, SSAManagementKey),
                                                                          EffectActionKey (AcquisitionalActionKey, SomaticAccessActionKey),
                                                                          GameState,
                                                                          Location,
                                                                          Object,
                                                                          Player,
                                                                          PlayerKey (PlayerKeyObject),
                                                                          SpatialRelationship (ContainedIn, Contains, SupportedBy, Supports))

-- Import action GIDs
import           Build.Identifiers.Actions                               (agentCanSeeGID,
                                                                          defaultInventoryLookFGID,
                                                                          dizzyGetFGID,
                                                                          dsvEnabledLookGID,
                                                                          getDeniedFGID,
                                                                          getFromChairFGID,
                                                                          getMailDeniedFGID,
                                                                          getRobeDeniedFGID,
                                                                          getRobeFGID,
                                                                          isaEnabledLookGID,
                                                                          lookAtChairFGID,
                                                                          lookAtRobeFGID,
                                                                          lookFGID,
                                                                          notEvenInventoryFGID,
                                                                          notEvenPillGID,
                                                                          notEvenRobeGID,
                                                                          openEyesGID,
                                                                          pillTooFarFGID,
                                                                          pitchBlackFGID,
                                                                          playerGetFGID,
                                                                          seeFloorFGID,
                                                                          seeMailGID,
                                                                          seePocketRobeWornGID,
                                                                          seeRobeChairGID,
                                                                          seeRobeWornGID,
                                                                          seeTableGID,
                                                                          takePillDeniedFGID,
                                                                          takePillFGID,
                                                                          whatChairFGID,
                                                                          whatPillGID,
                                                                          whatPocketGID)

-- Import verb functions
import           Grammar.Parser.Partitions.Nouns.Consumables             (pill)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs        (get)
import           Grammar.Parser.Partitions.Verbs.ConsumptionVerbs        (take)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb    (inventory,
                                                                          isaLook)
import           Grammar.Parser.Partitions.Verbs.SomaticAccessVerbs      (saOpen)

-- Import verb phrases
import           Build.Identifiers.Effects                               (youSeeMEffectGID)
import           Control.Monad                                           ((>=>))
import           Data.Foldable                                           (traverse_)
import           Debug.Trace                                             (trace)
import           GameState.ActionManagement                              (removeSystemEffect)
import           Model.Parser.Composites.Verbs                           (AcquisitionVerbPhrase (SimpleAcquisitionVerbPhrase),
                                                                          ConsumptionVerbPhrase (ConsumptionVerbPhrase))
import           Relude.Function                                         ((&))



-- =============================================================================
-- VERB PHRASES
-- =============================================================================

takePillCVP :: ConsumptionVerbPhrase
takePillCVP = ConsumptionVerbPhrase take (ConsumableNounPhrase (SimpleNounPhrase pill))

getMailAVP :: AcquisitionVerbPhrase
getMailAVP = SimpleAcquisitionVerbPhrase get (ObjectPhrase (SimpleNounPhrase mailOB))

getRobeAVP :: AcquisitionVerbPhrase
getRobeAVP = SimpleAcquisitionVerbPhrase get (ObjectPhrase (SimpleNounPhrase robeOB))

-- =============================================================================
-- OBJECT BUILDERS
-- =============================================================================

chairObj :: WorldDSL Object
chairObj =
  defaultObject & chairObj'
  where
    descriptives = [ SimpleNounPhrase chairDS
                   , DescriptiveNounPhraseDet the small chairDS
                   ]
    chairObj' = withShortName "a chair"
                  >=> withDescription "It's the chair next to your bed"
                  >=> withDescriptives descriptives
                  >=> (\o -> withObjectBehavior o (DSAManagementKey look whatChairFGID))
                  >=> (\o -> withObjectBehavior o (AVManagementKey  get  getFromChairFGID))

tableObj :: WorldDSL Object
tableObj =  defaultObject & tableObj'
  where
    descriptives = [ SimpleNounPhrase tableDS
                   , DescriptiveNounPhraseDet the small tableDS
                   ]
    tableObj' = withShortName "small table"
                  >=> withDescription "A small bedside table"
                  >=> withDescriptives descriptives
                  >=> (\o -> withObjectBehavior o (DSAManagementKey look seeTableGID))

pillObj :: WorldDSL Object
pillObj = defaultObject & pillObj'
  where
    pillObj' = withShortName "pill"
                 >=> withDescription "A small, round pill. Probably good for headaches."
                 >=> withDescriptives [ SimpleNounPhrase pillDS ]
                 >=> (\o -> withObjectBehavior o (DSAManagementKey look whatPillGID))
                 >=> (\o -> withObjectBehavior o (CVManagementKey takePillCVP takePillDeniedFGID))

mailObj :: WorldDSL Object
mailObj =  defaultObject & mailObj'
 where
   mailObj' = withShortName "mail"
                >=> withDescription "Some mail on the table"
                >=> withDescriptives [ SimpleNounPhrase mailDS ]
                >=> (\o -> withObjectBehavior o (DSAManagementKey look seeMailGID))
                >=> (\o -> withObjectBehavior o (AAManagementKey getMailAVP getMailDeniedFGID))

robeObj :: WorldDSL Object
robeObj =  defaultObject & robeObj'
  where
    robeObj' = withShortName "robe"
                 >=> withDescription "A comfortable robe"
                 >=> withDescriptives [ SimpleNounPhrase robeDS ]
                 >=> (\o -> withObjectBehavior o (DSAManagementKey look notEvenRobeGID))
                 >=> (\o -> withObjectBehavior o (AAManagementKey getRobeAVP getRobeDeniedFGID))
                 >=> (\o -> withObjectBehavior o (AVManagementKey  get        getRobeDeniedFGID))

pocketObj :: WorldDSL Object
pocketObj =  defaultObject & pocketObj'
  where
    pocketObj' = withShortName "pocket"
                   >=> withDescription "A pocket in the robe"
                   >=> withDescriptives [ SimpleNounPhrase pocketDS ]
                   >=> (\o -> withObjectBehavior o (DSAManagementKey look whatPocketGID))

floorObj :: WorldDSL Object
floorObj =  defaultObject & floorObj'
  where
    floorObj' = withShortName "floor"
                  >=> withDescription "The bedroom floor"
                  >=> withDescriptives [ SimpleNounPhrase floorDS ]
                  >=> (\o -> withObjectBehavior o (DSAManagementKey look seeFloorFGID))



-- =============================================================================
-- LOCATION AND PLAYER BUILDERS
-- =============================================================================

populateLocation :: GID Location
                    -> [(GID Object, NounKey)]
                    -> WorldDSL ()
populateLocation lid = traverse_ (uncurry (registerObjectToLocation lid))

-- objectsWithKeys -> [(GID Object, NounKey)]    -- ^ objects to place (object GID, noun key)
bedroomLoc :: WorldDSL Location
bedroomLoc = defaultLocation & bedroomLoc'
  where
    bedroomLoc' = withTitle "bedroom in bed"
                    >=> (\o -> withLocationBehavior o (ISAManagementKey isaLook pitchBlackFGID))

buildBedroomPlayer :: GID Location -> WorldDSL Player
buildBedroomPlayer bedroomGID =
     withPlayerLocation defaultPlayer bedroomGID
       >>= (\player -> withPlayerBehaviors player
                         [ ISAManagementKey isaLook isaEnabledLookGID
                         , ISAManagementKey inventory defaultInventoryLookFGID
                         , DSAManagementKey look dsvEnabledLookGID
                         , CVManagementKey takePillCVP pillTooFarFGID
                         , AAManagementKey getRobeAVP getDeniedFGID
                         , SSAManagementKey saOpen openEyesGID
                         ])

-- ============================================================================
-- MAIN DSL PROGRAM
-- =============================================================================

bedroomWorldDSL :: WorldDSL GameState
bedroomWorldDSL = do
  -- Declare GIDs using semantic wrappers
  bedroomGID <- declareLocationGID (SimpleNounPhrase bedroomDS)

  chairGID <- declareObjectGID (SimpleNounPhrase chairDS)
  tableGID <- declareObjectGID (DescriptiveNounPhraseDet the small tableDS)
  pillGID <- declareObjectGID (SimpleNounPhrase pillDS)
  mailGID <- declareObjectGID (SimpleNounPhrase mailDS)
  robeGID <- declareObjectGID (SimpleNounPhrase robeDS)
  pocketGID <- declareObjectGID (SimpleNounPhrase pocketDS)
  floorGID <- declareObjectGID (SimpleNounPhrase floorDS)

  -- Build and register objects
  registerObject chairGID chairObj
  registerObject tableGID tableObj
  registerObject pillGID pillObj
  registerObject mailGID mailObj
  registerObject robeGID robeObj
  registerObject pocketGID pocketObj
  registerObject floorGID floorObj

  -- Create and link effects for game actions
  openEyesLookChangesLookChair <- createDirectionalStimulusEffect look lookAtChairFGID
  linkEffectToObject ( SomaticAccessActionKey openEyesGID) chairGID openEyesLookChangesLookChair

  robeOpenEyesLookChangesLookRobe <- createDirectionalStimulusEffect look lookAtRobeFGID
  linkEffectToObject ( SomaticAccessActionKey openEyesGID) robeGID robeOpenEyesLookChangesLookRobe

  robeOpenEyesLookChangesGetRobe <- createAcquisitionVerbPhraseEffect getRobeAVP getRobeFGID
  linkEffectToObject ( SomaticAccessActionKey openEyesGID) robeGID robeOpenEyesLookChangesGetRobe

  trace ("DEBUG: getRobeFGID = " ++ show getRobeFGID) $ pure ()
  trace ("DEBUG: seeRobeWornGID = " ++ show seeRobeWornGID) $ pure ()
  getRobeChangesLookRobe <- createDirectionalStimulusEffect look lookAtRobeFGID
  trace ("DEBUG: Created getRobeChangesLookRobe effect: " ++ show getRobeChangesLookRobe) $ pure ()

-- Create field effect to change robe description when acquired
  robeHoldingDescriptionEffect <- updateDescription "A comfortable robe you are holding" robeGID
  linkFieldEffectToObject (AcquisitionalActionKey getRobeFGID) robeGID robeHoldingDescriptionEffect

  linkEffectToObject (AcquisitionalActionKey getRobeFGID) robeGID getRobeChangesLookRobe
  trace ("DEBUG: Linked effect to object " ++ show robeGID ++ " with key " ++ show (AcquisitionalActionKey getRobeFGID)) $ pure ()
--  tableLookEffect <- createDirectionalStimulusEffect look seeTableGID
--  linkEffectToObject (DirectionalStimulusActionKey seeTableGID) tableGID tableLookEffect

--  pillLookEffect <- createDirectionalStimulusEffect look whatPillGID
--  linkEffectToObject (DirectionalStimulusActionKey whatPillGID) pillGID pillLookEffect

--  mailLookEffect <- createDirectionalStimulusEffect look seeMailGID
--  linkEffectToObject (DirectionalStimulusActionKey seeMailGID) mailGID mailLookEffect

  getRobeEffect <- createAcquisitionVerbPhraseEffect getRobeAVP getRobeFGID
  linkEffectToObject (AcquisitionalActionKey getRobeFGID) robeGID getRobeEffect

--  takePillEffect <- createConsumptionEffect take pillGID takePillFGID
--  linkEffectToPlayer (ConsumptionActionKey takePillFGID) (PlayerKeyObject pillGID) takePillEffect

  robeOpenEyesLookChangesGetRobeForPlayer <- createAcquisitionVerbPhraseEffect getRobeAVP playerGetFGID
  linkEffectToPlayer (SomaticAccessActionKey openEyesGID) (PlayerKeyObject robeGID) robeOpenEyesLookChangesGetRobeForPlayer
  trace ("DEBUG: Created player effect: " ++ show robeOpenEyesLookChangesGetRobeForPlayer) $ pure ()
  trace ("DEBUG: Linking to player with key: " ++ show (SomaticAccessActionKey openEyesGID) ++ " and target: " ++ show (PlayerKeyObject robeGID)) $ pure ()
  -- Create the effect that changes look behavior when eyes open
  openEyesLookChangeEffect <- createImplicitStimulusEffect isaLook lookFGID

  robeOpenEyesChangesGetVerb <- createAcquisitionVerbEffect get getRobeFGID
  linkEffectToObject (SomaticAccessActionKey openEyesGID) robeGID robeOpenEyesChangesGetVerb
  linkEffectToLocation (SomaticAccessActionKey openEyesGID) bedroomGID openEyesLookChangeEffect

  openEyesSomaticEffect <- createSomaticAccessEffect saOpen openEyesGID
  linkEffectToLocation (SomaticAccessActionKey openEyesGID) bedroomGID openEyesSomaticEffect
  -- Complete spatial relationships
  registerSpatial chairGID (Supports (Set.singleton robeGID))
  registerSpatial chairGID (SupportedBy floorGID)
  registerSpatial tableGID (Supports (Set.singleton mailGID))
  registerSpatial tableGID (SupportedBy floorGID)
  registerSpatial mailGID (SupportedBy tableGID)
  registerSpatial robeGID (SupportedBy chairGID)
  registerSpatial robeGID (Contains (Set.singleton pocketGID))
  registerSpatial pocketGID (ContainedIn robeGID)
  registerSpatial pocketGID (Contains (Set.singleton pillGID))
  registerSpatial pillGID (ContainedIn pocketGID)
  registerSpatial floorGID (Supports (Set.fromList [chairGID, tableGID]))

  -- Register location with objects
  registerLocation bedroomGID bedroomLoc
  populateLocation bedroomGID
    [ (chairGID, DirectionalStimulusKey chairDS)
    , (tableGID, DirectionalStimulusKey tableDS)
    , (robeGID, DirectionalStimulusKey robeDS)
    , (mailGID, DirectionalStimulusKey mailDS)
    , (floorGID, DirectionalStimulusKey floorDS)
    , (chairGID, ObjectiveKey chairOB)
    , (tableGID, ObjectiveKey tableOB)
    , (robeGID, ObjectiveKey robeOB)
    , (mailGID, ObjectiveKey mailOB)
    , (floorGID, ObjectiveKey floorOB)
    ]
  setPerceptionMap
    [ (DirectionalStimulusNounPhrase (SimpleNounPhrase chairDS), [chairGID])
    , (DirectionalStimulusNounPhrase (SimpleNounPhrase tableDS), [tableGID])
    , (DirectionalStimulusNounPhrase (SimpleNounPhrase robeDS), [robeGID])
    , (DirectionalStimulusNounPhrase (SimpleNounPhrase mailDS), [mailGID])
    , (DirectionalStimulusNounPhrase (SimpleNounPhrase floorDS), [floorGID])
    ]
  trace "Perception map has been set" $ pure ()
--  displayAction <- displayVisibleObjects

  -- Link the open eyes action to trigger the system effect
--  linkActionKeyToSystemEffect (SomaticAccessActionKey openEyesGID) (SystemLocationKey bedroomGID)
  -- Create and register player
  player <- buildBedroomPlayer bedroomGID
  registerPlayer player

  finalizeGameState
