{-# LANGUAGE OverloadedStrings #-}

module Build.GameStateGeneration.BedroomWorldDSL where

import qualified Data.Set                                                as Set
import           Model.GameState.GameStateDSL                            (WorldDSL,
                                                                          addObjectToLocation,
                                                                          declareLocationGID,
                                                                          declareObjectGID,
                                                                          finalizeGameState,
                                                                          registerLocation,
                                                                          registerObject,
                                                                          registerPlayer,
                                                                          setSpatial,
                                                                          withDescription,
                                                                          withDescriptives,
                                                                          withObjectBehavior,
                                                                          withShortName,
                                                                          withTitle)
import           Model.GID                                               (GID)
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
                                                                          robeOB,
                                                                          tableOB)

-- Import adjectives and determiners
import           Grammar.Parser.Partitions.Adjectives                    (small)
import           Grammar.Parser.Partitions.Misc                          (the)
import           Model.Parser.Composites.Nouns                           (ConsumableNounPhrase (ConsumableNounPhrase),
                                                                          NounPhrase (DescriptiveNounPhraseDet, SimpleNounPhrase),
                                                                          ObjectPhrase (ObjectPhrase))

-- Import ObjectSpec builder functions

-- Import behavior management constructors and spatial relationships
import           Model.GameState                                         (ActionManagement (AAManagementKey, AVManagementKey, CAManagementKey, DSAManagementKey, ISAManagementKey, SSAManagementKey),
                                                                          GameState,
                                                                          Location,
                                                                          Object,
                                                                          Player,
                                                                          SpatialRelationship (ContainedIn, Contains, SupportedBy, Supports))

-- Import action GIDs
import           Build.Identifiers.Actions                               (checkInventoryGID,
                                                                          dsvEnabledLookGID,
                                                                          getFromChairFGID,
                                                                          getMailDeniedFGID,
                                                                          getRobeDeniedFGID,
                                                                          isaEnabledLookGID,
                                                                          notEvenRobeGID,
                                                                          openEyesGID,
                                                                          pillTooFarFGID,
                                                                          seeChairFGID,
                                                                          seeFloorFGID,
                                                                          seeMailGID,
                                                                          seePocketRobeWornGID,
                                                                          seeTableGID,
                                                                          takePillDeniedFGID,
                                                                          whatPillGID)

-- Import verb functions
import           Grammar.Parser.Partitions.Nouns.Consumables             (pill)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs        (get)
import           Grammar.Parser.Partitions.Verbs.ConsumptionVerbs        (take)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb    (inventory,
                                                                          isaLook)
import           Grammar.Parser.Partitions.Verbs.SomaticAccessVerbs      (saOpen)

-- Import verb phrases
import           Build.GameStateGeneration.ObjectSpec                    (defaultLocation,
                                                                          defaultObject,
                                                                          defaultPlayer,
                                                                          withLocationBehaviors,
                                                                          withObjects,
                                                                          withPlayerBehaviors,
                                                                          withPlayerLocation)
import           Control.Monad                                           ((>=>))
import           Data.Foldable                                           (traverse_)
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

chairObj ::WorldDSL Object
chairObj = defaultObject & chairObj'
  where
    chairObj' = withShortName "a chair"
                  >=> withDescription "It's the chair next to your bed"
                  >=> withDescriptives [SimpleNounPhrase chairDS, DescriptiveNounPhraseDet the small chairDS]

tableObj :: WorldDSL Object
tableObj = defaultObject & tableObj'
  where
    tableObj' = withShortName "small table"
                  >=> withDescription "A small bedside table"
                  >=> withDescriptives [DescriptiveNounPhraseDet the small tableDS]

pillObj :: WorldDSL Object
pillObj = defaultObject & pillObj'
  where
    pillObj' = withShortName "pill"
                 >=> withDescription "A small, round pill. Probably good for headaches."
                 >=> withDescriptives [SimpleNounPhrase pillDS]

mailObj :: WorldDSL Object
mailObj = defaultObject & mailObj'
  where
    mailObj' = withShortName "mail"
                 >=> withDescription "Some mail on the table"
                 >=> withDescriptives [SimpleNounPhrase mailDS]


robeObj :: WorldDSL Object
robeObj = defaultObject & robeObj'
  where
    robeObj' = withShortName "robe"
                 >=> withDescription "A comfortable robe"
                 >=> withDescriptives [SimpleNounPhrase robeDS]

pocketObj :: WorldDSL Object
pocketObj = defaultObject & pocketObj'
  where
    pocketObj' = withShortName "pocket"
                   >=> withDescription "A pocket in the robe"
                   >=> withDescriptives [SimpleNounPhrase pocketDS]


floorObj :: WorldDSL Object
floorObj =
  defaultObject & floorObj'
  where
    floorObj' = withShortName "floor"
                  >=> withDescription "The bedroom floor"
                  >=> withDescriptives [ SimpleNounPhrase floorDS ]



-- =============================================================================
-- LOCATION AND PLAYER BUILDERS
-- =============================================================================

buildBedroom
  :: GID Location               -- ^ target Location GID
  -> [(GID Object, NounKey)]    -- ^ objects to place (object GID, noun key)
  -> WorldDSL ()
buildBedroom bedroomGID objectsWithKeys = do
  -- Prepare the spec for the location.
  let loc = defaultLocation
          & withTitle "Bedroom in Bed"

  -- 1) Register the location under its GID.
  _ <- registerLocation bedroomGID loc

  -- 2) Link all required objects for this location.
  traverse_ (uncurry (addObjectToLocation bedroomGID))
            objectsWithKeys

buildBedroomPlayer :: GID Location -> WorldDSL Player
buildBedroomPlayer bedroomGID = do
  let player = defaultPlayer
        & withPlayerLocation bedroomGID
        & withPlayerBehaviors
            [ ISAManagementKey isaLook isaEnabledLookGID
            , ISAManagementKey inventory checkInventoryGID
            , DSAManagementKey look dsvEnabledLookGID
            , CAManagementKey takePillCVP pillTooFarFGID
            , SSAManagementKey saOpen openEyesGID
            ]
  return player

-- =============================================================================
-- MAIN DSL PROGRAM
-- =============================================================================

bedroomWorldDSL :: WorldDSL GameState
bedroomWorldDSL = do
  -- Declare GIDs using semantic wrappers
  bedroomGID <- declareLocationGID (SimpleNounPhrase bedroomDS)
  chairGID <- declareObjectGID (SimpleNounPhrase chairDS)
  tableGID <- declareObjectGID (DescriptiveNounPhraseDet the small tableDS)  -- "the small table" living in DSL
  pillGID <- declareObjectGID (SimpleNounPhrase pillDS)
  mailGID <- declareObjectGID (SimpleNounPhrase mailDS)
  robeGID <- declareObjectGID (SimpleNounPhrase robeDS)
  pocketGID <- declareObjectGID (SimpleNounPhrase pocketDS)
  floorGID <- declareObjectGID (SimpleNounPhrase floorDS)

  -- Build and register objects with behaviors using DSL chaining
  chairWithBehaviors <- do
    chairWithLook <- withObjectBehavior chairObj (DSAManagementKey look seeChairFGID)
    withObjectBehavior chairWithLook (AVManagementKey get getFromChairFGID)
  registerObject chairGID chairWithBehaviors

  tableWithBehaviors <- do
    withObjectBehavior tableObj (DSAManagementKey look seeTableGID)
  registerObject tableGID tableWithBehaviors

  pillWithBehaviors <- do
    pillWithLook <- withObjectBehavior pillObj (DSAManagementKey look whatPillGID)
    withObjectBehavior pillWithLook (CAManagementKey takePillCVP takePillDeniedFGID)
  registerObject pillGID pillWithBehaviors

  mailWithBehaviors <- do
    mailWithLook <- withObjectBehavior mailObj (DSAManagementKey look seeMailGID)
    withObjectBehavior mailWithLook (AAManagementKey getMailAVP getMailDeniedFGID)
  registerObject mailGID mailWithBehaviors

  robeWithBehaviors <- do
    robeWithLook <- withObjectBehavior robeObj (DSAManagementKey look notEvenRobeGID)
    robeWithAcquisition <- withObjectBehavior robeWithLook (AAManagementKey getRobeAVP getRobeDeniedFGID)
    withObjectBehavior robeWithAcquisition (AVManagementKey get getRobeDeniedFGID)
  registerObject robeGID robeWithBehaviors

  pocketWithBehaviors <- do
    withObjectBehavior pocketObj (DSAManagementKey look seePocketRobeWornGID)
  registerObject pocketGID pocketWithBehaviors

  floorWithBehaviors <- do
    withObjectBehavior floorObj (DSAManagementKey look seeFloorFGID)
  registerObject floorGID floorWithBehaviors

  -- Set up spatial relationships
  setSpatial chairGID (Supports (Set.singleton robeGID))
  setSpatial chairGID (SupportedBy floorGID)
  setSpatial tableGID (Supports (Set.singleton mailGID))
  setSpatial tableGID (SupportedBy floorGID)
  setSpatial mailGID (SupportedBy tableGID)
  setSpatial robeGID (SupportedBy chairGID)
  setSpatial robeGID (Contains (Set.singleton pocketGID))
  setSpatial pocketGID (ContainedIn robeGID)
  setSpatial pocketGID (Contains (Set.singleton pillGID))
  setSpatial pillGID (ContainedIn pocketGID)
  setSpatial floorGID (Supports (Set.fromList [chairGID, tableGID]))

  -- Build and register location
  bedroomLoc <- buildBedroom bedroomGID
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
  registerLocation bedroomGID bedroomLoc

  -- Create and set player
  player <- buildBedroomPlayer bedroomGID
  registerPlayer player

  finalizeGameState

