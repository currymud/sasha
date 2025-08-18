module Build.GameStateGeneration.BedroomWorldDSL where

import           Data.Function                                           ((&))
import qualified Data.Set                                                as Set
import           Model.GameState                                         (GameState,
                                                                          Location,
                                                                          Object,
                                                                          Player,
                                                                          SpatialRelationship (ContainedIn, Contains, SupportedBy, Supports))
import           Model.GameState.GameStateDSL                            (WorldDSL,
                                                                          buildLocation,
                                                                          buildObject,
                                                                          buildPlayer,
                                                                          createAAManagement,
                                                                          createAVManagement,
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

-- Import semantic wrappers
import           Grammar.Parser.Partitions.Nouns.Consumables             (pillCS)
import           Grammar.Parser.Partitions.Nouns.Containers              (pocketCT,
                                                                          robeCT)
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
import           Grammar.Parser.Partitions.Nouns.Surfaces                (chairSF,
                                                                          floorSF,
                                                                          tableSF)

-- Import adjectives and determiners
import           Grammar.Parser.Partitions.Adjectives                    (small)
import           Grammar.Parser.Partitions.Misc                          (the)
import           Model.Parser.Composites.Nouns                           (NounPhrase (DescriptiveNounPhraseDet, SimpleNounPhrase))

-- Raw lexemes for "the small table" (living in DSL for now)
import           Build.GameStateGeneration.ObjectSpec                    (defaultLocation,
                                                                          defaultObject,
                                                                          defaultPlayer,
                                                                          withDescription,
                                                                          withDescriptives,
                                                                          withObjects,
                                                                          withPlayerLocation,
                                                                          withShortName,
                                                                          withTitle)
import           Build.GameStateGeneration.ObjectSpec.Objects            (getRobeAVP,
                                                                          takePillCVP)
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
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus     (table)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs        (get)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (dsaLook)
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb    (isaLook)

-- TODO: Need to import these from appropriate modules
-- dsaLook, get, isaLook, etc.
-- seeChairFGID, getFromChairFGID, etc.
-- takePillCVP, getMailAVP, getRobeAVP
-- DirectionalStimulusKey, ObjectiveKey

-- Main DSL program
bedroomWorldDSL :: WorldDSL GameState
bedroomWorldDSL = do
  -- Declare GIDs using semantic wrappers
  bedroomGID <- declareLocationGID (SimpleNounPhrase bedroomDS)
  chairGID <- declareObjectGID (SimpleNounPhrase chairDS)
  tableGID <- declareObjectGID (DescriptiveNounPhraseDet the small table)  -- "the small table" living in DSL
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
    , (tableGID, DirectionalStimulusKey table)
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
buildChair chairGID robeGID floorGID =
  buildObject (pure chairGID) (initialObject defaultObject) chairBuilder
    `withBehavior` (createDSAManagement (pure dsaLook) (pure seeChairFGID))
    `withBehavior` (createAVManagement (pure get) (pure getFromChairFGID))
    `withSpatial` [Supports (Set.singleton robeGID), SupportedBy floorGID]
  where
    chairBuilder = withShortName "a chair"
                   & withDescription "It's the chair next to your bed"
                   & withDescriptives [SimpleNounPhrase chairDS, DescriptiveNounPhraseDet the small chairDS]

buildTable :: GID Object -> GID Object -> GID Object -> WorldDSL Object
buildTable tableGID mailGID floorGID =
  buildObject (pure tableGID) (initialObject defaultObject) tableBuilder
    `withBehavior` (createDSAManagement (pure dsaLook) (pure seeTableGID))
    `withSpatial` [Supports (Set.singleton mailGID), SupportedBy floorGID]
  where
    tableBuilder = withShortName "small table"
                   & withDescription "A small bedside table"
                   & withDescriptives [DescriptiveNounPhraseDet the small table]

buildPill :: GID Object -> GID Object -> WorldDSL Object
buildPill pillGID pocketGID =
  buildObject (pure pillGID) (initialObject defaultObject) pillBuilder
    `withBehavior` (createDSAManagement (pure dsaLook) (pure whatPillGID))
    `withBehavior` (createCAManagement (pure takePillCVP) (pure takePillDeniedFGID))
    `withSpatial` [ContainedIn pocketGID]
  where
    pillBuilder = withShortName "pill"
                  & withDescription "A small, round pill. Probably good for headaches."
                  & withDescriptives [SimpleNounPhrase pillCS]

buildMail :: GID Object -> GID Object -> WorldDSL Object
buildMail mailGID tableGID =
  buildObject (pure mailGID) (initialObject defaultObject) mailBuilder
    `withBehavior` (createDSAManagement (pure dsaLook) (pure seeMailGID))
    `withBehavior` (createAAManagement (pure getMailAVP) (pure getMailDeniedFGID))
    `withSpatial` [SupportedBy tableGID]
  where
    mailBuilder = withShortName "mail"
                  & withDescription "Some mail on the table"
                  & withDescriptives [SimpleNounPhrase mailDS]

buildRobe :: GID Object -> GID Object -> GID Object -> WorldDSL Object
buildRobe robeGID chairGID pocketGID =
  buildObject (pure robeGID) (initialObject defaultObject) robeBuilder
    `withBehavior` (createDSAManagement (pure dsaLook) (pure seeRobeChairGID))
    `withBehavior` (createAAManagement (pure getRobeAVP) (pure dizzyGetFGID))
    `withSpatial` [SupportedBy chairGID, Contains (Set.singleton pocketGID)]
  where
    robeBuilder = withShortName "robe"
                  & withDescription "A comfortable robe"
                  & withDescriptives [SimpleNounPhrase robeCT]

buildPocket :: GID Object -> GID Object -> GID Object -> WorldDSL Object
buildPocket pocketGID robeGID pillGID =
  buildObject (pure pocketGID) (initialObject defaultObject) pocketBuilder
    `withBehavior` (createDSAManagement (pure dsaLook) (pure seePocketRobeWornGID))
    `withSpatial` [ContainedIn robeGID, Contains (Set.singleton pillGID)]
  where
    pocketBuilder = withShortName "pocket"
                    & withDescription "A pocket in the robe"
                    & withDescriptives [SimpleNounPhrase pocketCT]

buildFloor :: GID Object -> GID Object -> GID Object -> WorldDSL Object
buildFloor floorGID chairGID tableGID =
  buildObject (pure floorGID) (initialObject defaultObject) floorBuilder
    `withBehavior` (createDSAManagement (pure dsaLook) (pure seeFloorFGID))
    `withSpatial` [Supports (Set.fromList [chairGID, tableGID])]
  where
    floorBuilder = withShortName "floor"
                   & withDescription "The bedroom floor"
                   & withDescriptives [SimpleNounPhrase floorSF]

-- Location builder with improved ergonomics
buildBedroom :: GID Location -> [(GID Object, NounKey)] -> WorldDSL Location
buildBedroom locationGID objectList = do
  buildLocation (pure locationGID) (initialLocation defaultLocation) bedroomBuilder
  where
    bedroomBuilder = withTitle "Bedroom in Bed"
                     & withObjects objectList

-- Player builder
buildBedroomPlayer :: GID Location -> WorldDSL Player
buildBedroomPlayer locationGID = do
  buildPlayer (initialPlayer defaultPlayer) playerBuilder
    `withPlayerBehavior` (createISAManagement (pure isaLook) (pure isaEnabledLookGID))
    `withPlayerBehavior` (createDSAManagement (pure dsaLook) (pure dsvEnabledLookGID))
    `withPlayerBehavior` (createCAManagement (pure takePillCVP) (pure pillTooFarFGID))
  where
    playerBuilder = withPlayerLocation locationGID
