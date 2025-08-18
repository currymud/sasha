{-# LANGUAGE OverloadedStrings #-}

module Build.GameStateGeneration.BedroomWorldDSL where

import qualified Data.Set                                                as Set
import           Model.GameState.GameStateDSL                            (WorldDSL,
                                                                          buildLocation,
                                                                          buildObject,
                                                                          buildPlayer,
                                                                          createAAManagement,
                                                                          createCAManagement,
                                                                          createDSAManagement,
                                                                          createISAManagement,
                                                                          declareLocationGID,
                                                                          declareObjectGID,
                                                                          finalizeGameState,
                                                                          initialLocation,
                                                                          initialObject,
                                                                          initialPlayer,
                                                                          processEffectsIntoRegistry,
                                                                          registerLocation,
                                                                          registerObject,
                                                                          setPlayer,
                                                                          withBehavior,
                                                                          withPlayerBehavior,
                                                                          withSpatial)
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
import           Grammar.Parser.Partitions.Nouns.Objectives              (bedroomOB,
                                                                          chairOB,
                                                                          floorOB,
                                                                          mailOB,
                                                                          pillOB,
                                                                          pocketOB,
                                                                          robeOB,
                                                                          tableOB)

-- Import adjectives and determiners
import           Grammar.Parser.Partitions.Adjectives                    (small)
import           Grammar.Parser.Partitions.Misc                          (the)
import           Model.Parser.Composites.Nouns                           (NounPhrase (DescriptiveNounPhraseDet, SimpleNounPhrase))

-- Import ObjectSpec builder functions
import           Build.GameStateGeneration.ObjectSpec                    (defaultLocation,
                                                                          defaultObject,
                                                                          defaultPlayer,
                                                                          withDescription,
                                                                          withDescriptives,
                                                                          withObjects,
                                                                          withPlayerLocation,
                                                                          withShortName,
                                                                          withTitle)

-- Import behavior management constructors and spatial relationships
import           Model.GameState                                         (GameState,
                                                                          Location,
                                                                          Object,
                                                                          Player,
                                                                          SpatialRelationship (ContainedIn, Contains, SupportedBy, Supports))

-- Import action GIDs
import           Build.Identifiers.Actions                               (dizzyGetFGID,
                                                                          dsvEnabledLookGID,
                                                                          getFromChairFGID,
                                                                          getMailDeniedFGID,
                                                                          isaEnabledLookGID,
                                                                          pillTooFarFGID,
                                                                          seeChairFGID,
                                                                          seeFloorFGID,
                                                                          seeMailGID,
                                                                          seePocketRobeWornGID,
                                                                          seeRobeChairGID,
                                                                          seeTableGID,
                                                                          takePillDeniedFGID,
                                                                          whatPillGID)

-- Import verb phrases
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs        (get)
import           Grammar.Parser.Partitions.Verbs.ConsumptionVerbs        (take)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (dsaLook)
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb    (isaLook)
import           Model.Parser.Composites.Nouns                           (ConsumableNounPhrase (ConsumableNounPhrase),
                                                                          ObjectPhrase (ObjectPhrase))
import           Model.Parser.Composites.Verbs                           (AcquisitionVerbPhrase (SimpleAcquisitionVerbPhrase),
                                                                          ConsumptionVerbPhrase (ConsumptionVerbPhrase))

-- Import the correct consumable version
import           Data.Function                                           ((&))
import           Grammar.Parser.Partitions.Nouns.Consumables             (pillCS)


-- Create the required verb phrases
takePillCVP :: ConsumptionVerbPhrase
takePillCVP = ConsumptionVerbPhrase take (ConsumableNounPhrase (SimpleNounPhrase pillCS))

getMailAVP :: AcquisitionVerbPhrase
getMailAVP = SimpleAcquisitionVerbPhrase get (ObjectPhrase (SimpleNounPhrase mailOB))

getRobeAVP :: AcquisitionVerbPhrase
getRobeAVP = SimpleAcquisitionVerbPhrase get (ObjectPhrase (SimpleNounPhrase robeOB))

-- Main DSL program
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

  -- Build and register objects using new chaining pattern
  chairObj <- buildChair chairGID robeGID floorGID
  tableObj <- buildTable tableGID mailGID floorGID
  pillObj <- buildPill pillGID pocketGID
  mailObj <- buildMail mailGID tableGID
  robeObj <- buildRobe robeGID chairGID pocketGID
  pocketObj <- buildPocket pocketGID robeGID pillGID
  floorObj <- buildFloor floorGID chairGID tableGID

  registerObject (pure chairGID) (pure chairObj)
  registerObject (pure tableGID) (pure tableObj)
  registerObject (pure pillGID) (pure pillObj)
  registerObject (pure mailGID) (pure mailObj)
  registerObject (pure robeGID) (pure robeObj)
  registerObject (pure pocketGID) (pure pocketObj)
  registerObject (pure floorGID) (pure floorObj)

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
  registerLocation (pure bedroomGID) (pure bedroomLoc)

  -- Create and set player
  player <- buildBedroomPlayer bedroomGID
  setPlayer (pure player)

  processEffectsIntoRegistry
  finalizeGameState

-- Updated build functions using withBehavior and withSpatial chaining
buildChair :: GID Object -> GID Object -> GID Object -> WorldDSL Object
buildChair chairGID robeGID floorGID = do
  behavior1 <- createDSAManagement (pure dsaLook) (pure seeChairFGID)
  obj <- buildObject (pure chairGID) (initialObject defaultObject) chairBuilder
  objWithBehavior <- withBehavior obj behavior1
  withSpatial objWithBehavior [Supports (Set.singleton robeGID), SupportedBy floorGID]
  where
    chairBuilder = withShortName "a chair"
                   . withDescription "It's the chair next to your bed"
                   . withDescriptives [SimpleNounPhrase chairDS, DescriptiveNounPhraseDet the small chairDS]

buildTable :: GID Object -> GID Object -> GID Object -> WorldDSL Object
buildTable tableGID mailGID floorGID = do
  behavior1 <- createDSAManagement (pure dsaLook) (pure seeTableGID)
  obj <- buildObject (pure tableGID) (initialObject defaultObject) tableBuilder
  objWithBehavior <- withBehavior obj behavior1
  withSpatial objWithBehavior [Supports (Set.singleton mailGID), SupportedBy floorGID]
  where
    tableBuilder = withShortName "small table"
                   . withDescription "A small bedside table"
                   . withDescriptives [DescriptiveNounPhraseDet the small tableDS]

buildPill :: GID Object -> GID Object -> WorldDSL Object
buildPill pillGID pocketGID = do
  behavior1 <- createDSAManagement (pure dsaLook) (pure whatPillGID)
  behavior2 <- createCAManagement (pure takePillCVP) (pure takePillDeniedFGID)
  obj <- buildObject (pure pillGID) (initialObject defaultObject) pillBuilder
  objWithBehavior1 <- withBehavior obj behavior1
  objWithBehavior2 <- withBehavior objWithBehavior1 behavior2
  withSpatial objWithBehavior2 [ContainedIn pocketGID]
  where
    pillBuilder = withShortName "pill"
                  . withDescription "A small, round pill. Probably good for headaches."
                  . withDescriptives [SimpleNounPhrase pillDS]

buildMail :: GID Object -> GID Object -> WorldDSL Object
buildMail mailGID tableGID = do
  behavior1 <- createDSAManagement (pure dsaLook) (pure seeMailGID)
  behavior2 <- createAAManagement (pure getMailAVP) (pure getMailDeniedFGID)
  obj <- buildObject (pure mailGID) (initialObject defaultObject) mailBuilder
  objWithBehavior1 <- withBehavior obj behavior1
  objWithBehavior2 <- withBehavior objWithBehavior1 behavior2
  withSpatial objWithBehavior2 [SupportedBy tableGID]
  where
    mailBuilder = withShortName "mail"
                  . withDescription "Some mail on the table"
                  . withDescriptives [SimpleNounPhrase mailDS]

buildRobe :: GID Object -> GID Object -> GID Object -> WorldDSL Object
buildRobe robeGID chairGID pocketGID = do
  behavior1 <- createDSAManagement (pure dsaLook) (pure seeRobeChairGID)
  behavior2 <- createAAManagement (pure getRobeAVP) (pure dizzyGetFGID)
  obj <- buildObject (pure robeGID) (initialObject defaultObject) robeBuilder
  objWithBehavior1 <- withBehavior obj behavior1
  objWithBehavior2 <- withBehavior objWithBehavior1 behavior2
  withSpatial objWithBehavior2 [SupportedBy chairGID, Contains (Set.singleton pocketGID)]
  where
    robeBuilder = withShortName "robe"
                  . withDescription "A comfortable robe"
                  . withDescriptives [SimpleNounPhrase robeDS]

buildPocket :: GID Object -> GID Object -> GID Object -> WorldDSL Object
buildPocket pocketGID robeGID pillGID = do
  behavior1 <- createDSAManagement (pure dsaLook) (pure seePocketRobeWornGID)
  obj <- buildObject (pure pocketGID) (initialObject defaultObject) pocketBuilder
  objWithBehavior <- withBehavior obj behavior1
  withSpatial objWithBehavior [ContainedIn robeGID, Contains (Set.singleton pillGID)]
  where
    pocketBuilder = withShortName "pocket"
                    . withDescription "A pocket in the robe"
                    . withDescriptives [SimpleNounPhrase pocketDS]

buildFloor :: GID Object -> GID Object -> GID Object -> WorldDSL Object
buildFloor floorGID chairGID tableGID = do
  behavior1 <- createDSAManagement (pure dsaLook) (pure seeFloorFGID)
  obj <- buildObject (pure floorGID) (initialObject defaultObject) floorBuilder
  objWithBehavior <- withBehavior obj behavior1
  withSpatial objWithBehavior [Supports (Set.fromList [chairGID, tableGID])]
  where
    floorBuilder = withShortName "floor"
                   . withDescription "The bedroom floor"
                   . withDescriptives [SimpleNounPhrase floorDS]

-- Location builder with improved ergonomics
buildBedroom :: GID Location -> [(GID Object, NounKey)] -> WorldDSL Location
buildBedroom locationGID objectList = do
  buildLocation (pure locationGID) (initialLocation defaultLocation) bedroomBuilder
  where
    bedroomBuilder = withTitle "Bedroom in Bed"
                     . withObjects objectList

-- Player builder
buildBedroomPlayer :: GID Location -> WorldDSL Player
buildBedroomPlayer locationGID = do
  behavior1 <- createISAManagement (pure isaLook) (pure isaEnabledLookGID)
  behavior2 <- createDSAManagement (pure dsaLook) (pure dsvEnabledLookGID)
  behavior3 <- createCAManagement (pure takePillCVP) (pure pillTooFarFGID)
  player <- buildPlayer (initialPlayer defaultPlayer) playerBuilder
  playerWithBehavior1 <- withPlayerBehavior (pure player) (pure behavior1)
  playerWithBehavior2 <- withPlayerBehavior (pure playerWithBehavior1) (pure behavior2)
  withPlayerBehavior (pure playerWithBehavior2) (pure behavior3)
  where
    playerBuilder = withPlayerLocation locationGID
