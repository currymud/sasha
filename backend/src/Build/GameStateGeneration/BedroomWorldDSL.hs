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

  registerObject chairGID chairObj
  registerObject tableGID tableObj
  registerObject pillGID pillObj
  registerObject mailGID mailObj
  registerObject robeGID robeObj
  registerObject pocketGID pocketObj
  registerObject floorGID floorObj

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
  setPlayer player

  processEffectsIntoRegistry
  finalizeGameState

