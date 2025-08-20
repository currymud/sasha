{-# LANGUAGE OverloadedStrings #-}

module Build.GameStateGeneration.BedroomWorldDSL where

import qualified Data.Set                                                as Set
import           Model.GameState                                         (ImplicitStimulusActionF,
                                                                          SystemEffect (PerceptionSystemEffect))
import           Model.GameState.GameStateDSL                            (WorldDSL,
                                                                          createAcquisitionPhraseEffect,
                                                                          createConsumptionEffect,
                                                                          createDirectionalStimulusEffect,
                                                                          createImplicitStimulusEffect,
                                                                          createSomaticAccessEffect,
                                                                          declareLocationGID,
                                                                          declareObjectGID,
                                                                          displayVisibleObjects,
                                                                          finalizeGameState,
                                                                          linkEffectToLocation,
                                                                          linkEffectToObject,
                                                                          linkEffectToPlayer,
                                                                          linkSystemEffectToAction,
                                                                          registerLocation,
                                                                          registerObject,
                                                                          registerObjectToLocation,
                                                                          registerPlayer,
                                                                          registerSpatial,
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
                                                                          NounPhrase (DescriptiveNounPhraseDet, SimpleNounPhrase),
                                                                          ObjectPhrase (ObjectPhrase))

-- Import ObjectSpec builder functions

-- Import behavior management constructors and spatial relationships
import           Model.GameState                                         (ActionKey (SomaticAccessActionKey),
                                                                          ActionManagement (AAManagementKey, AVManagementKey, CAManagementKey, DSAManagementKey, ISAManagementKey, SSAManagementKey),
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
                                                                          notEvenPillGID,
                                                                          notEvenRobeGID,
                                                                          openEyesGID,
                                                                          pillTooFarFGID,
                                                                          pitchBlackFGID,
                                                                          robeCollectedFGID,
                                                                          seeChairFGID,
                                                                          seeFloorFGID,
                                                                          seeMailGID,
                                                                          seePocketRobeWornGID,
                                                                          seeTableGID,
                                                                          takePillDeniedFGID,
                                                                          takePillFGID,
                                                                          whatPillGID)

import           Build.Identifiers.Actions                               (isaEnabledLookGID,
                                                                          pitchBlackFGID)



-- Import verb functions
import           Grammar.Parser.Partitions.Nouns.Consumables             (pill)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs        (get)
import           Grammar.Parser.Partitions.Verbs.ConsumptionVerbs        (take)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb    (inventory,
                                                                          isaLook)
import           Grammar.Parser.Partitions.Verbs.SomaticAccessVerbs      (saOpen)

-- Import verb phrases
import           Build.GameStateGeneration.Defaults                      (defaultLocation,
                                                                          defaultObject,
                                                                          defaultPlayer)
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
bedroomLoc = defaultLocation & bedroomLoc'
  where
    bedroomLoc' = withTitle "bedroom in bed"
                    >=> (\o -> withLocationBehavior o (ISAManagementKey isaLook pitchBlackFGID))

buildBedroomPlayer :: GID Location -> WorldDSL Player
buildBedroomPlayer bedroomGID =
     withPlayerLocation defaultPlayer bedroomGID
       >>= (\player -> withPlayerBehaviors player
                         [ ISAManagementKey isaLook isaEnabledLookGID
                         , ISAManagementKey inventory checkInventoryGID
                         , DSAManagementKey look dsvEnabledLookGID
                         , CAManagementKey takePillCVP pillTooFarFGID
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

  chairLookEffect <- createDirectionalStimulusEffect look seeChairFGID
  linkEffectToObject chairGID chairLookEffect

  tableLookEffect <- createDirectionalStimulusEffect look seeTableGID
  linkEffectToObject tableGID tableLookEffect

  pillLookEffect <- createDirectionalStimulusEffect look whatPillGID
  linkEffectToObject pillGID pillLookEffect

  mailLookEffect <- createDirectionalStimulusEffect look seeMailGID
  linkEffectToObject mailGID mailLookEffect

  getRobeEffect <- createAcquisitionPhraseEffect getRobeAVP robeCollectedFGID
  linkEffectToObject robeGID getRobeEffect

  takePillEffect <- createConsumptionEffect take pillGID takePillFGID
  linkEffectToPlayer (PlayerKeyObject pillGID) takePillEffect

  openEyesImplicitStimulusEffect <- createImplicitStimulusEffect isaLook agentCanSeeGID
  linkEffectToLocation bedroomGID openEyesImplicitStimulusEffect

  openEyesSomaticEffect <- createSomaticAccessEffect saOpen openEyesGID
  linkEffectToLocation bedroomGID openEyesSomaticEffect

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
  displayAction <- displayVisibleObjects
  linkSystemEffectToAction (SomaticAccessActionKey openEyesGID) (PerceptionSystemEffect displayAction)

  -- Create and register player
  player <- buildBedroomPlayer bedroomGID
  registerPlayer player

  finalizeGameState
