module Build.GameStateGeneration.BedroomWorldSpec (bedroomWorldSpec, bedroomPlayerSpec) where


import qualified Grammar.Parser.Partitions.Nouns.Consumables             as Consumables
import qualified Grammar.Parser.Partitions.Nouns.Objectives              as Objectives
import qualified Grammar.Parser.Partitions.Verbs.AcquisitionVerbs        as AcquisitionVerbs
import qualified Grammar.Parser.Partitions.Verbs.ConsumptionVerbs        as ConsumptionVerbs

import           Build.GameStateGeneration.LocationSpec.LocationGIDs     (bedroomGID)
import           Build.GameStateGeneration.LocationSpec.Locations        (bedroom)
import           Build.GameStateGeneration.ObjectSpec.ObjectGIDS         (chairGID,
                                                                          floorGID,
                                                                          mailGID,
                                                                          pillGID,
                                                                          pocketGID,
                                                                          robeGID,
                                                                          smallTableGID)

import           Build.GameStateGeneration.WorldGeneration               (ObjectBehaviorSpec (ObjectBehaviorSpec, _behaviors, _objectGID),
                                                                          PlayerSpec (PlayerSpec, _playerBehaviors, _playerLocationGID),
                                                                          WorldSpec (WorldSpec, _locationSpecs, _objectSpecs, _playerSpec, _spatialRelationships))
import           Build.Identifiers.Actions                               (checkInventoryGID,
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
import qualified Data.Set
import           Grammar.Parser.Partitions.Nouns.Objectives              (robeOB)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs        (get)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (dsaLook)
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb    (inventory,
                                                                          isaLook)
import           Grammar.Parser.Partitions.Verbs.SomaticAccessVerbs      (saOpen)
import           Model.GameState                                         (ActionManagement (AAManagementKey, CAManagementKey, DSAManagementKey, ISAManagementKey, SSAManagementKey),
                                                                          Location,
                                                                          Object,
                                                                          SpatialRelationship (ContainedIn, Contains, SupportedBy, Supports))
import           Model.GID                                               (GID)
import           Model.Parser.Atomics.Nouns                              (Objective)
import           Model.Parser.Composites.Nouns                           (ConsumableNounPhrase (ConsumableNounPhrase),
                                                                          NounPhrase (SimpleNounPhrase),
                                                                          ObjectPhrase (ObjectPhrase))
import           Model.Parser.Composites.Verbs                           (AcquisitionVerbPhrase (AcquisitionVerbPhrase, SimpleAcquisitionVerbPhrase),
                                                                          ConsumptionVerbPhrase (ConsumptionVerbPhrase))



chairBehaviorSpec :: ObjectBehaviorSpec
chairBehaviorSpec = ObjectBehaviorSpec
  { _objectGID = chairGID
  , _behaviors =
      [ DSAManagementKey dsaLook seeChairFGID ]
  }

tableBehaviorSpec :: ObjectBehaviorSpec
tableBehaviorSpec = ObjectBehaviorSpec
  { _objectGID = smallTableGID
  , _behaviors =
      [ DSAManagementKey dsaLook seeTableGID ]
  }

pillBehaviorSpec :: ObjectBehaviorSpec
pillBehaviorSpec = ObjectBehaviorSpec
  { _objectGID = pillGID
  , _behaviors =
      [ DSAManagementKey dsaLook whatPillGID
      , CAManagementKey takePillCVP takePillDeniedFGID
      ]
  }

mailBehaviorSpec :: ObjectBehaviorSpec
mailBehaviorSpec = ObjectBehaviorSpec
  { _objectGID = mailGID
  , _behaviors =
      [ DSAManagementKey dsaLook seeMailGID
      , AAManagementKey getMailAVP getMailDeniedFGID
      ]
  }

robeBehaviorSpec :: ObjectBehaviorSpec
robeBehaviorSpec = ObjectBehaviorSpec
  { _objectGID = robeGID
  , _behaviors =
      [ DSAManagementKey dsaLook seeRobeChairGID
      , AAManagementKey getRobeAVP dizzyGetFGID
      ]
  }

pocketBehaviorSpec :: ObjectBehaviorSpec
pocketBehaviorSpec = ObjectBehaviorSpec
  { _objectGID = pocketGID
  , _behaviors =
      [ DSAManagementKey dsaLook seePocketRobeWornGID ]
  }

floorBehaviorSpec :: ObjectBehaviorSpec
floorBehaviorSpec = ObjectBehaviorSpec
  { _objectGID = floorGID
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
  [ (chairGID, [Supports (Data.Set.singleton robeGID), SupportedBy floorGID])
  , (smallTableGID, [Supports (Data.Set.singleton mailGID), SupportedBy floorGID])
  , (mailGID, [SupportedBy smallTableGID])
  , (robeGID, [SupportedBy chairGID, Contains (Data.Set.singleton pocketGID)])
  , (pocketGID, [ContainedIn robeGID, Contains (Data.Set.singleton pillGID)])
  , (pillGID, [ContainedIn pocketGID])
  , (floorGID, [Supports (Data.Set.fromList [chairGID, smallTableGID])])
  ]

-- Location specifications for the bedroom
bedroomLocationSpecs :: [(GID Location, Location)]
bedroomLocationSpecs =
  [(bedroomGID, bedroom)]

-- PlayerSpec for the bedroom scenario
bedroomPlayerSpec :: PlayerSpec
bedroomPlayerSpec = PlayerSpec
  { _playerLocationGID = bedroomGID
  , _playerBehaviors =
      [
        ISAManagementKey isaLook isaEnabledLookGID
      , ISAManagementKey inventory checkInventoryGID

      , DSAManagementKey dsaLook dsvEnabledLookGID

      , CAManagementKey takePillCVP pillTooFarFGID
      , SSAManagementKey saOpen openEyesGID
      ]
  }

-- =============================================================================
-- ACQUISITION VERB PHRASES
-- =============================================================================

takePillCVP :: ConsumptionVerbPhrase
takePillCVP = ConsumptionVerbPhrase
  ConsumptionVerbs.take
  (ConsumableNounPhrase (SimpleNounPhrase Consumables.pill))

getMailAVP :: AcquisitionVerbPhrase
getMailAVP = SimpleAcquisitionVerbPhrase
  AcquisitionVerbs.get
  (ObjectPhrase (SimpleNounPhrase Objectives.mail))

simpleRobeOP :: ObjectPhrase
simpleRobeOP = ObjectPhrase (SimpleNounPhrase robeOB)

getRobeAVP :: AcquisitionVerbPhrase
getRobeAVP = SimpleAcquisitionVerbPhrase get simpleRobeOP

