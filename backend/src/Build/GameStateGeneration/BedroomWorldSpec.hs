module Build.GameStateGeneration.BedroomWorldSpec (bedroomWorldSpec, bedroomPlayerSpec) where
import           Build.BedPuzzle.Actions.Objects.Mail                 (getMailAVP)
import           Build.BedPuzzle.Actions.Objects.Pill                 (takePillCVP)
import           Build.BedPuzzle.BedRoom                              (bedroomInBed)
import           Build.GameStateGeneration.WorldBuilder               (dirLook,
                                                                       dsaLook,
                                                                       getRobeAVP,
                                                                       isaLook,
                                                                       saOpen)
import           Build.GameStateGeneration.WorldGeneration            (ObjectBehaviorSpec (ObjectBehaviorSpec, _behaviors, _objectGID),
                                                                       PlayerSpec (PlayerSpec, _playerBehaviors, _playerLocationGID),
                                                                       WorldSpec (WorldSpec, _locationSpecs, _objectSpecs, _playerSpec, _spatialRelationships))
import           Build.Identifiers.Actions                            (checkInventoryGID,
                                                                       dizzyGetFGID,
                                                                       dsvEnabledLookGID,
                                                                       getMailDeniedFGID,
                                                                       isaEnabledLookGID,
                                                                       openEyesGID,
                                                                       pillTooFarFGID,
                                                                       seeChairFGID,
                                                                       seeFloorFGID,
                                                                       seeMailGID,
                                                                       seePocketRobeWornGID,
                                                                       seeRobeChairGID,
                                                                       seeTableGID,
                                                                       takePillDeniedFGID,
                                                                       whatPillGID)
import           Build.Identifiers.Locations                          (bedroomInBedGID)
import           Build.Identifiers.Objects                            (chairObjGID,
                                                                       floorObjGID,
                                                                       mailObjGID,
                                                                       pillObjGID,
                                                                       pocketObjGID,
                                                                       robeObjGID,
                                                                       tableObjGID)
import qualified Data.Set
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb (inventory)
import           Model.GameState                                      (ActionManagement (AAManagementKey, CAManagementKey, DSAManagementKey, ISAManagementKey, SSAManagementKey),
                                                                       Location,
                                                                       Object,
                                                                       SpatialRelationship (ContainedIn, Contains, SupportedBy, Supports))
import           Model.GID                                            (GID)


chairBehaviorSpec :: ObjectBehaviorSpec
chairBehaviorSpec = ObjectBehaviorSpec
  { _objectGID = chairObjGID
  , _behaviors =
      [ DSAManagementKey dsaLook seeChairFGID ]
  }

tableBehaviorSpec :: ObjectBehaviorSpec
tableBehaviorSpec = ObjectBehaviorSpec
  { _objectGID = tableObjGID
  , _behaviors =
      [ DSAManagementKey dsaLook seeTableGID ]
  }

pillBehaviorSpec :: ObjectBehaviorSpec
pillBehaviorSpec = ObjectBehaviorSpec
  { _objectGID = pillObjGID
  , _behaviors =
      [ DSAManagementKey dsaLook whatPillGID
      , CAManagementKey takePillCVP takePillDeniedFGID
      ]
  }

mailBehaviorSpec :: ObjectBehaviorSpec
mailBehaviorSpec = ObjectBehaviorSpec
  { _objectGID = mailObjGID
  , _behaviors =
      [ DSAManagementKey dsaLook seeMailGID
      , AAManagementKey getMailAVP getMailDeniedFGID
      ]
  }

robeBehaviorSpec :: ObjectBehaviorSpec
robeBehaviorSpec = ObjectBehaviorSpec
  { _objectGID = robeObjGID
  , _behaviors =
      [ DSAManagementKey dsaLook seeRobeChairGID
      , AAManagementKey getRobeAVP dizzyGetFGID
      ]
  }

pocketBehaviorSpec :: ObjectBehaviorSpec
pocketBehaviorSpec = ObjectBehaviorSpec
  { _objectGID = pocketObjGID
  , _behaviors =
      [ DSAManagementKey dsaLook seePocketRobeWornGID ]
  }

floorBehaviorSpec :: ObjectBehaviorSpec
floorBehaviorSpec = ObjectBehaviorSpec
  { _objectGID = floorObjGID
  , _behaviors =
      [ DSAManagementKey dsaLook seeFloorFGID ]
  }

-- Bedroom world specification using individual behavior specs
bedroomWorldSpec :: WorldSpec
bedroomWorldSpec = WorldSpec
  { _objectSpecs = allBedroomObjectSpecs
  , _spatialRelationships = bedroomSpatialRelationships
  , _locationSpecs = bedroomLocationSpecs
  , _playerSpec = bedroomPlayerSpec
  }

-- All object behavior specs for the bedroom
allBedroomObjectSpecs :: [ObjectBehaviorSpec]
allBedroomObjectSpecs =
  [ chairBehaviorSpec
  , tableBehaviorSpec
  , pillBehaviorSpec
  , mailBehaviorSpec
  , robeBehaviorSpec
  , pocketBehaviorSpec
  , floorBehaviorSpec
  ]

-- Spatial relationships for the bedroom scene
bedroomSpatialRelationships :: [(GID Object, [SpatialRelationship])]
bedroomSpatialRelationships =
  [ (chairObjGID, [Supports (Data.Set.singleton robeObjGID), SupportedBy floorObjGID])
  , (tableObjGID, [Supports (Data.Set.singleton mailObjGID), SupportedBy floorObjGID])
  , (mailObjGID, [SupportedBy tableObjGID])
  , (robeObjGID, [SupportedBy chairObjGID, Contains (Data.Set.singleton pocketObjGID)])
  , (pocketObjGID, [ContainedIn robeObjGID, Contains (Data.Set.singleton pillObjGID)])
  , (pillObjGID, [ContainedIn pocketObjGID])
  , (floorObjGID, [Supports (Data.Set.fromList [chairObjGID, tableObjGID])])
  ]

-- Location specifications for the bedroom
bedroomLocationSpecs :: [(GID Location, Location)]
bedroomLocationSpecs =
  [(bedroomInBedGID, bedroomInBed)]

-- PlayerSpec for the bedroom scenario
bedroomPlayerSpec :: PlayerSpec
bedroomPlayerSpec = PlayerSpec
  { _playerLocationGID = bedroomInBedGID
  , _playerBehaviors =
      [
        ISAManagementKey isaLook isaEnabledLookGID
      , ISAManagementKey inventory checkInventoryGID

      , DSAManagementKey dsaLook dsvEnabledLookGID

      , CAManagementKey takePillCVP pillTooFarFGID
      , SSAManagementKey saOpen openEyesGID
      ]
  }
