{-# LANGUAGE OverloadedStrings #-}

module Build.GameStateGeneration.BedroomWorldDSL where

import qualified Data.Set                                                as Set
import           Model.GameState.GameStateDSL                            (WorldDSL,
                                                                          createDirectionalStimulusEffect,
                                                                          createImplicitStimulusEffect,
                                                                          declareLocationGID,
                                                                          declareObjectGID,
                                                                          finalizeGameState,
                                                                          linkEffectToLocation,
                                                                          linkEffectToObject,
                                                                          registerObject,
                                                                          registerObjectToLocation,
                                                                          registerPlayer,
                                                                          registerSpatial,
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
                                                                          pillOB,
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
                                                                          PlayerKey (PlayerKeyObject),
                                                                          SpatialRelationship (ContainedIn, Contains, SupportedBy, Supports))

-- Import action GIDs
import           Build.Identifiers.Actions                               (agentCanSeeGID,
                                                                          checkInventoryGID,
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
                  >=> (\o -> withObjectBehavior o (DSAManagementKey look seeChairFGID))
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
                 >=> (\o -> withObjectBehavior o (CAManagementKey takePillCVP takePillDeniedFGID))

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
                   >=> (\o -> withObjectBehavior o (DSAManagementKey look seePocketRobeWornGID))

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
bedroomLoc = do
  defaultLocation
    & withTitle "Bedroom in Bed"

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
  -- Look effects for objects
  chairLookEffect <- createDirectionalStimulusEffect look seeChairFGID
  linkEffectToObject chairGID chairLookEffect

  tableLookEffect <- createDirectionalStimulusEffect look seeTableGID
  linkEffectToObject tableGID tableLookEffect

  pillLookEffect <- createDirectionalStimulusEffect look whatPillGID
  linkEffectToObject pillGID pillLookEffect

  mailLookEffect <- createDirectionalStimulusEffect look seeMailGID
  linkEffectToObject mailGID mailLookEffect

  -- Acquisition effects
--  getRobeEffect <- createAcquisitionEffect getRobeAVP robeCollectedFGID
--  linkEffectToObject robeGID getRobeEffect

  -- Consumption effects
--  takePillEffect <- createConsumptionEffect take pillGID takePillFGID
--  linkEffectToPlayer (PlayerKeyObject pillGID) takePillEffect

  -- Opening eyes effect (game start)
  openEyesEffect <- createImplicitStimulusEffect isaLook agentCanSeeGID
  linkEffectToLocation bedroomGID openEyesEffect

  -- Set up spatial relationships
  registerSpatial chairGID (Supports (Set.singleton robeGID))
  registerSpatial chairGID (SupportedBy floorGID)
  -- ... other spatial relationships

  -- Build and register location
--  bedroomLoc' <- buildBedroom bedroomGID objectsWithKeys
--  registerLocation bedroomGID bedroomLoc'

  -- Create and set player
  player <- buildBedroomPlayer bedroomGID
  registerPlayer player

  finalizeGameState
