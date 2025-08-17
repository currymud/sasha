module Build.GameStateGeneration.BedroomWorldDSL where

import           Data.Function                ((&))
import qualified Data.Set                     as Set
import           Model.GameState              (GameState, Location, Object)
import           Model.GameState.GameStateDSL
import           Model.GID                    (GID)

-- Main DSL program
bedroomWorldDSL :: WorldDSL GameState
bedroomWorldDSL = do
  -- Declare GIDs
  bedroomGID <- declareLocationGID (SimpleNounPhrase bedroom)
  chairGID <- declareObjectGID (SimpleNounPhrase chair)
  tableGID <- declareObjectGID (DescriptiveNounPhraseDet the small table)
  pillGID <- declareObjectGID (SimpleNounPhrase pill)
  mailGID <- declareObjectGID (SimpleNounPhrase mail)
  robeGID <- declareObjectGID (SimpleNounPhrase robe)
  pocketGID <- declareObjectGID (SimpleNounPhrase pocket)
  floorGID <- declareObjectGID (SimpleNounPhrase floor)

  -- Build and register objects
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
  bedroomLoc <- buildBedroom bedroomGID chairGID tableGID robeGID mailGID floorGID
  registerLocation (pure bedroomGID) (pure bedroomLoc)

  -- Create and set player
  player <- buildBedroomPlayer bedroomGID
  setPlayer (pure player)

  processEffectsIntoRegistry
  finalizeGameState

buildChair :: GID Object -> GID Object -> GID Object -> WorldDSL Object
buildChair gid robeGID floorGID = do
  obj <- buildObject (pure gid) (initialObject defaultObject) chairBuilder
  assignObjectBehavior (pure gid) (createDSAManagement (pure dsaLook) (pure seeChairFGID))
  assignObjectBehavior (pure gid) (createAVManagement (pure get) (pure getFromChairFGID))
  setSpatial (pure gid) (pure [Supports (Set.singleton robeGID), SupportedBy floorGID])
  pure obj
  where
    chairBuilder = withShortName "a chair"
                   & withDescription "It's the chair next to your bed"
                   & withDescriptives [SimpleNounPhrase chair, DescriptiveNounPhraseDet the small chair]

buildTable :: GID Object -> GID Object -> GID Object -> WorldDSL Object
buildTable gid mailGID floorGID = do
  obj <- buildObject (pure gid) (initialObject defaultObject) tableBuilder
  assignObjectBehavior (pure gid) (createDSAManagement (pure dsaLook) (pure seeTableGID))
  setSpatial (pure gid) (pure [Supports (Set.singleton mailGID), SupportedBy floorGID])
  pure obj
  where
    tableBuilder = withShortName "small table"
                   & withDescription "A small bedside table"
                   & withDescriptives [DescriptiveNounPhraseDet the small table]

buildPill :: GID Object -> GID Object -> WorldDSL Object
buildPill gid pocketGID = do
  obj <- buildObject (pure gid) (initialObject defaultObject) pillBuilder
  assignObjectBehavior (pure gid) (createDSAManagement (pure dsaLook) (pure whatPillGID))
  assignObjectBehavior (pure gid) (createCAManagement (pure takePillCVP) (pure takePillDeniedFGID))
  setSpatial (pure gid) (pure [ContainedIn pocketGID])
  pure obj
  where
    pillBuilder = withShortName "pill"
                  & withDescription "A small, round pill. Probably good for headaches."
                  & withDescriptives [SimpleNounPhrase pill]

buildMail :: GID Object -> GID Object -> WorldDSL Object
buildMail gid tableGID = do
  obj <- buildObject (pure gid) (initialObject defaultObject) mailBuilder
  assignObjectBehavior (pure gid) (createDSAManagement (pure dsaLook) (pure seeMailGID))
  assignObjectBehavior (pure gid) (createAAManagement (pure getMailAVP) (pure getMailDeniedFGID))
  setSpatial (pure gid) (pure [SupportedBy tableGID])
  pure obj
  where
    mailBuilder = withShortName "mail"
                  & withDescription "Some mail on the table"
                  & withDescriptives [SimpleNounPhrase mail]

buildRobe :: GID Object -> GID Object -> GID Object -> WorldDSL Object
buildRobe gid chairGID pocketGID = do
  obj <- buildObject (pure gid) (initialObject defaultObject) robeBuilder
  assignObjectBehavior (pure gid) (createDSAManagement (pure dsaLook) (pure seeRobeChairGID))
  assignObjectBehavior (pure gid) (createAAManagement (pure getRobeAVP) (pure dizzyGetFGID))
  setSpatial (pure gid) (pure [SupportedBy chairGID, Contains (Set.singleton pocketGID)])
  pure obj
  where
    robeBuilder = withShortName "robe"
                  & withDescription "A comfortable robe"
                  & withDescriptives [SimpleNounPhrase robe]

buildPocket :: GID Object -> GID Object -> GID Object -> WorldDSL Object
buildPocket gid robeGID pillGID = do
  obj <- buildObject (pure gid) (initialObject defaultObject) pocketBuilder
  assignObjectBehavior (pure gid) (createDSAManagement (pure dsaLook) (pure seePocketRobeWornGID))
  setSpatial (pure gid) (pure [ContainedIn robeGID, Contains (Set.singleton pillGID)])
  pure obj
  where
    pocketBuilder = withShortName "pocket"
                    & withDescription "A pocket in the robe"
                    & withDescriptives [SimpleNounPhrase pocket]

buildFloor :: GID Object -> GID Object -> GID Object -> WorldDSL Object
buildFloor gid chairGID tableGID = do
  obj <- buildObject (pure gid) (initialObject defaultObject) floorBuilder
  assignObjectBehavior (pure gid) (createDSAManagement (pure dsaLook) (pure seeFloorFGID))
  setSpatial (pure gid) (pure [Supports (Set.fromList [chairGID, tableGID])])
  pure obj
  where
    floorBuilder = withShortName "floor"
                   & withDescription "The bedroom floor"
                   & withDescriptives [SimpleNounPhrase floor]

-- Location builder
buildBedroom :: GID Location -> GID Object -> GID Object -> GID Object -> GID Object -> GID Object -> WorldDSL Location
buildBedroom bedroomGID chairGID tableGID robeGID mailGID floorGID = do
  loc <- buildLocation (pure bedroomGID) (initialLocation defaultLocation) bedroomBuilder
  assignLocationBehavior (pure bedroomGID) (createISAManagement (pure isaLook) (pure pitchBlackFGID))
  pure loc
  where
    bedroomBuilder = withTitle "Bedroom in Bed"
                     & withObjects [ (chairGID, DirectionalStimulusKey chair)
                                   , (tableGID, DirectionalStimulusKey table)
                                   , (robeGID, DirectionalStimulusKey robe)
                                   , (mailGID, DirectionalStimulusKey mail)
                                   , (floorGID, DirectionalStimulusKey floor)
                                   , (chairGID, ObjectiveKey Objectives.chair)
                                   , (tableGID, ObjectiveKey Objectives.table)
                                   , (robeGID, ObjectiveKey Objectives.robe)
                                   , (mailGID, ObjectiveKey Objectives.mail)
                                   , (floorGID, ObjectiveKey Objectives.floor)
                                   ]

-- Player builder
buildBedroomPlayer :: GID Location -> WorldDSL Player
buildBedroomPlayer bedroomGID = do
  basePlayer <- buildPlayer (initialPlayer defaultPlayer) (withPlayerLocation bedroomGID)

  player1 <- assignPlayerBehavior (pure basePlayer)
    (createISAManagement (pure isaLook) (pure isaEnabledLookGID))

  player2 <- assignPlayerBehavior (pure player1)
    (createISAManagement (pure inventory) (pure checkInventoryGID))

  player3 <- assignPlayerBehavior (pure player2)
    (createDSAManagement (pure dsaLook) (pure dsvEnabledLookGID))

  player4 <- assignPlayerBehavior (pure player3)
    (createCAManagement (pure takePillCVP) (pure pillTooFarFGID))

  assignPlayerBehavior (pure player4)
    (createSSAManagement (pure saOpen) (pure openEyesGID))

-- Spatial relationships
-- (No longer needed - handled in object builders)

-- Helper phrases
takePillCVP :: ConsumptionVerbPhrase
takePillCVP = ConsumptionVerbPhrase take (ConsumableNounPhrase (SimpleNounPhrase Consumables.pill))

getMailAVP :: AcquisitionVerbPhrase
getMailAVP = SimpleAcquisitionVerbPhrase get (ObjectPhrase (SimpleNounPhrase Objectives.mail))

getRobeAVP :: AcquisitionVerbPhrase
getRobeAVP = SimpleAcquisitionVerbPhrase get (ObjectPhrase (SimpleNounPhrase Objectives.robe))
