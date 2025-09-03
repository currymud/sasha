{-# LANGUAGE OverloadedStrings #-}

module Build.GameStateGeneration.SashaDemo where

import           Build.GameStateGeneration.Defaults                               (defaultLocation,
                                                                                   defaultObject,
                                                                                   defaultPlayer)
import           Prelude                                                          hiding
                                                                                  (take)

-- Import semantic wrappers - DirectionalStimulus versions
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus              (bedroomDS,
                                                                                   chairDS,
                                                                                   floorDS,
                                                                                   pocketDS,
                                                                                   robeDS)

-- Import adjectives and determiners
import           Model.Parser.Composites.Nouns                                    (ConsumableNounPhrase (ConsumableNounPhrase),
                                                                                   DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                                                   NounPhrase (SimpleNounPhrase),
                                                                                   ObjectPhrase (ObjectPhrase))

-- Import behavior management constructors and spatial relationships
import           Model.GameState                                                  (AcquisitionActionF,
                                                                                   ActionEffectKey,
                                                                                   ActionManagement (AAManagementKey, AVManagementKey, DSAManagementKey, ISAManagementKey, SSAManagementKey),
                                                                                   ActionManagementFunctions,
                                                                                   DirectionalStimulusActionF,
                                                                                   Effect,
                                                                                   EffectActionKey (AcquisitionalActionKey, SomaticAccessActionKey),
                                                                                   GameState,
                                                                                   ImplicitStimulusActionF,
                                                                                   Location,
                                                                                   Object,
                                                                                   Player,
                                                                                   PlayerKey (PlayerKeyObject),
                                                                                   SomaticAccessActionF,
                                                                                   SpatialRelationship (SupportedBy, Supports))
import           Model.GameState.GameStateDSL                                     (WorldDSL,
                                                                                   createAcquisitionVerbEffect,
                                                                                   createAcquisitionVerbPhraseEffect,
                                                                                   createDirectionalStimulusEffect,
                                                                                   createImplicitStimulusEffect,
                                                                                   declareAcquisitionActionGID,
                                                                                   declareDirectionalStimulusActionGID,
                                                                                   declareImplicitStimulusActionGID,
                                                                                   declareLocationGID,
                                                                                   declareObjectGID,
                                                                                   declareSomaticActionGID,
                                                                                   finalizeGameState,
                                                                                   linkEffectToLocation,
                                                                                   linkEffectToObject,
                                                                                   linkEffectToPlayer,
                                                                                   linkFieldEffectToObject,
                                                                                   registerLocation,
                                                                                   registerObject,
                                                                                   registerObjectToLocation,
                                                                                   registerPlayer,
                                                                                   registerSpatial,
                                                                                   setPerceptionMap,
                                                                                   updateDescription,
                                                                                   withDescription,
                                                                                   withDescriptives,
                                                                                   withLocationBehavior,
                                                                                   withObjectBehavior,
                                                                                   withPlayerBehavior,
                                                                                   withPlayerLocation,
                                                                                   withShortName,
                                                                                   withTitle)
-- Import verb functions
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb             (isaLook)
import           Grammar.Parser.Partitions.Verbs.SomaticAccessVerbs               (saOpen)

-- Import verb phrases
import           Relude.Function                                                  ((&))

-- Import action functions from BedPuzzle
import           Build.BedPuzzle.Actions.Get                                      (getDeniedF,
                                                                                   getF)
import           Build.BedPuzzle.Actions.Get.Constructors                         (getFromSupportF,
                                                                                   getObjectF)
import           Build.BedPuzzle.Actions.Locations.Look                           (lookF,
                                                                                   pitchBlackF)
import           Build.BedPuzzle.Actions.Look                                     (lookAtF)
import           Build.BedPuzzle.Actions.Objects.Chair.Look                       (whatChairF)
import           Build.BedPuzzle.Actions.Objects.Pill.Look                        (whatPill)
import           Build.BedPuzzle.Actions.Objects.Pocket.Look                      (whatPocket)
import           Build.BedPuzzle.Actions.Objects.Robe.Get                         (getRobeDeniedF)
import           Build.BedPuzzle.Actions.Objects.Robe.Look                        (notEvenRobeF)
import           Build.BedPuzzle.Actions.Open                                     (openEyes)
import           Build.BedPuzzle.Actions.Player.Look                              (dsvActionEnabled,
                                                                                   isvActionEnabled)
import           Control.Monad                                                    ((>=>))
import           Data.Kind                                                        (Type)
import qualified Data.Set
import           Data.Text                                                        (Text)
import           GameState.ActionManagement                                       (findAVKey,
                                                                                   findDSAKey)
import           Grammar.Parser.Partitions.Nouns.Consumables                      (pill,
                                                                                   pillCS)
import           Grammar.Parser.Partitions.Nouns.Objectives                       (chairOB,
                                                                                   floorOB,
                                                                                   mailOB,
                                                                                   pillOB,
                                                                                   robeOB)
import           Grammar.Parser.Partitions.Prepositions.DirectionalStimulusMarker (at)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs                 (get,
                                                                                   take)
import           Grammar.Parser.Partitions.Verbs.ConsumptionVerbs                 (takeCV)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb          (dsaLook,
                                                                                   look)
import           Model.GID                                                        (GID)
import           Model.Parser.Atomics.Nouns                                       (DirectionalStimulus,
                                                                                   Objective)
import           Model.Parser.Composites.Verbs                                    (AcquisitionVerbPhrase (SimpleAcquisitionVerbPhrase),
                                                                                   ConsumptionVerbPhrase (ConsumptionVerbPhrase))
import           Model.Parser.GCase                                               (NounKey (DirectionalStimulusKey, ObjectiveKey))

-- =============================================================================
-- VERB PHRASES
-- =============================================================================

takePillCVP :: ConsumptionVerbPhrase
takePillCVP = ConsumptionVerbPhrase takeCV (ConsumableNounPhrase (SimpleNounPhrase pillCS))

getMailAVP :: AcquisitionVerbPhrase
getMailAVP = SimpleAcquisitionVerbPhrase get (ObjectPhrase (SimpleNounPhrase mailOB))

getRobeAVP :: AcquisitionVerbPhrase-- Objects are built and contribute their actions to ActionMaps
getRobeAVP = SimpleAcquisitionVerbPhrase get (ObjectPhrase (SimpleNounPhrase robeOB))

type ObjectSpec :: Type
type ObjectSpec =
  ( GID Object                     -- The object's GID
  , WorldDSL Object                -- Builder with behaviors
  )

type EffectSpec :: Type
type EffectSpec =
  ( EffectActionKey      -- Trigger: what action causes this
  , ActionEffectKey      -- Target: LocationKey | ObjectKey | PlayerKey
  , WorldDSL Effect      -- The effect builder
  )

type ObjectBuildError :: Type
data ObjectBuildError
  = MissingAction Text
  | InvalidActionGID Text
  deriving stock (Show,Ord, Eq)

buildObject :: NounPhrase DirectionalStimulus
            -> (ActionManagementFunctions -> Either ObjectBuildError (WorldDSL Object))
            -> ActionManagementFunctions
            -> WorldDSL (GID Object, Object)
buildObject nounPhrase objBuilder actions =
  case objBuilder actions of
    Left err -> error $ "Object build failed: " ++ show err  -- Crash here!
    Right objDSL -> do
      gid <- declareObjectGID nounPhrase
      obj <- objDSL
      return (gid, obj)

chairObj :: ActionManagementFunctions -> Either ObjectBuildError (WorldDSL Object)
chairObj actions = do  -- Either monad
  lookGID <- maybeToEither (MissingAction "Missing look action for chair")
                          (findDSAKey look actions)
  getGID <- maybeToEither (MissingAction "Missing get action for chair")
                         (findAVKey get actions)
  return $ defaultObject & chairBuilder lookGID getGID

chairBuilder :: GID DirectionalStimulusActionF
                  -> GID AcquisitionActionF
                  -> Object -> WorldDSL Object
chairBuilder lookGID getGID = withShortName "chair"
  >=> withDescription "A simple wooden chair"
  >=> withDescriptives [SimpleNounPhrase chairDS]
  >=> (\o -> withObjectBehavior o (DSAManagementKey look lookGID))
  >=> (\o -> withObjectBehavior o (AVManagementKey get getGID))

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither err Nothing = Left err
maybeToEither _ (Just a)  = Right a


    {-
testDynamicActionsDS :: WorldDSL GameState
testDynamicActionsDSL = do
  -- Declare location GID
  bedroomGID <- declareLocationGID (SimpleNounPhrase bedroomDS)

-- Object GIDs
  floorGID <- declareObjectGID (SimpleNounPhrase floorDS)
  chairGID <- declareObjectGID (SimpleNounPhrase chairDS)
  robeGID <- declareObjectGID (SimpleNounPhrase robeDS)
  pocketGID <- declareObjectGID (SimpleNounPhrase pocketDS)

  lookAtChairFGID <- declareDirectionalStimulusActionGID (lookAtF chairGID)
  whatChairGID <- declareDirectionalStimulusActionGID whatChairF
  getFromChairFGID <- declareAcquisitionActionGID (getFromSupportF chairGID)

  whatPocketFGID <- declareDirectionalStimulusActionGID whatPocket
  lookAtPocketFGID <- declareDirectionalStimulusActionGID (lookAtF pocketGID)

  lookAtRobeFGID <- declareDirectionalStimulusActionGID (lookAtF robeGID)
  notEvenRobeFGID <- declareDirectionalStimulusActionGID notEvenRobeF
  getRobeDeniedFGID <- declareAcquisitionActionGID getRobeDeniedF
  getRobeFGID <- declareAcquisitionActionGID (getObjectF robeGID)

  lookFloorGID <- declareDirectionalStimulusActionGID (lookAtF floorGID)

  whatPillFGID <- declareDirectionalStimulusActionGID whatPill

  registerObject chairGID (chairObj whatChairGID getFromChairFGID)
  registerObject floorGID (floorObj lookFloorGID)
  registerObject robeGID (robeObj notEvenRobeFGID getRobeDeniedFGID)
  registerObject pocketGID (pocketObj whatPocketFGID)
  -- Generate open eyes action GIDs dynamically
  openEyesGID <- declareSomaticActionGID openEyes
  getDeniedFGID <- declareAcquisitionActionGID getDeniedF
  playerGetFGID <- declareAcquisitionActionGID getF
  pitchBlackGID <- declareImplicitStimulusActionGID pitchBlackF
  lookFGID <- declareImplicitStimulusActionGID lookF
  isaEnabledLookGID <- declareImplicitStimulusActionGID (isvActionEnabled isaLook)
  dsvEnabledLookGID <- declareDirectionalStimulusActionGID dsvActionEnabled

  registerLocation bedroomGID (buildLocation pitchBlackGID)

  placeObject bedroomGID chairGID chairDS chairOB
  placeObject bedroomGID floorGID floorDS floorOB
  placeObject bedroomGID robeGID robeDS robeOB

  registerSpatial chairGID (SupportedBy floorGID)
  registerSpatial floorGID (Supports (Data.Set.singleton chairGID))
  registerSpatial chairGID (Supports (Data.Set.singleton robeGID))
  registerSpatial robeGID (Supports (Data.Set.singleton pocketGID))
  registerSpatial robeGID (SupportedBy chairGID)
  registerSpatial pocketGID (SupportedBy robeGID)
--  registerObject floorGID (floorObj id)
  player <- buildBedroomPlayer bedroomGID isaEnabledLookGID openEyesGID dsvEnabledLookGID getDeniedFGID

  openEyesLookChangeEffect <- createImplicitStimulusEffect isaLook lookFGID
  linkEffectToLocation (SomaticAccessActionKey openEyesGID) bedroomGID openEyesLookChangeEffect

  openEyesLookChangesLookChair <- createDirectionalStimulusEffect look lookAtChairFGID
  linkEffectToObject (SomaticAccessActionKey openEyesGID) chairGID openEyesLookChangesLookChair

  robeOpenEyesLookChangesLookRobe <- createDirectionalStimulusEffect look lookAtRobeFGID
  linkEffectToObject ( SomaticAccessActionKey openEyesGID) robeGID robeOpenEyesLookChangesLookRobe

  openEyesLookChangesGetRobe <- createAcquisitionVerbPhraseEffect getRobeAVP getRobeFGID
  linkEffectToObject ( SomaticAccessActionKey openEyesGID) robeGID openEyesLookChangesGetRobe

  robeOpenEyesLookChangesGetRobeForPlayer <- createAcquisitionVerbPhraseEffect getRobeAVP playerGetFGID
  linkEffectToPlayer (SomaticAccessActionKey openEyesGID) (PlayerKeyObject robeGID) robeOpenEyesLookChangesGetRobeForPlayer

  openEyesChangeGetVerb <- createAcquisitionVerbEffect get getRobeFGID
  linkEffectToObject (SomaticAccessActionKey openEyesGID) robeGID openEyesChangeGetVerb

  getRobeChangesLookAtPocket <- createDirectionalStimulusEffect look lookAtPocketFGID
  linkEffectToObject (AcquisitionalActionKey getRobeFGID) pocketGID getRobeChangesLookAtPocket

  robeHoldingDescriptionEffect <- updateDescription "A comfortable robe you are holding. You notice it has a pocket - that seems important for some reason." robeGID
  linkFieldEffectToObject (AcquisitionalActionKey getRobeFGID) robeGID robeHoldingDescriptionEffect

  registerPlayer player
  setPerceptionMap
    [ (DirectionalStimulusNounPhrase at (SimpleNounPhrase chairDS), [chairGID])
--    , (DirectionalStimulusNounPhrase (SimpleNounPhrase tableDS), [tableGID])
    , (DirectionalStimulusNounPhrase at (SimpleNounPhrase robeDS), [robeGID])
--    , (DirectionalStimulusNounPhrase (SimpleNounPhrase mailDS), [mailGID])
    , (DirectionalStimulusNounPhrase at (SimpleNounPhrase floorDS), [floorGID])
    ]

  finalizeGameState

buildLocation :: GID ImplicitStimulusActionF -> WorldDSL Location
buildLocation pitchBlackGID = defaultLocation & bedroomLoc'
  where
    bedroomLoc' = withTitle "bedroom in bed"
                    >=> (\l -> withLocationBehavior l (ISAManagementKey isaLook pitchBlackGID))

buildBedroomPlayer :: GID Location
                        -> GID ImplicitStimulusActionF
                        -> GID SomaticAccessActionF
                        -> GID DirectionalStimulusActionF
                        -> GID AcquisitionActionF
                        -> WorldDSL Player
buildBedroomPlayer bedroomGID isaEnabledLookGID openEyesGID dsaEnabledLookGID getRobeFGID =
  withPlayerLocation defaultPlayer bedroomGID
    >>= (\p -> withPlayerBehavior p (ISAManagementKey isaLook isaEnabledLookGID))
    >>= (\p -> withPlayerBehavior p (DSAManagementKey look dsaEnabledLookGID))
    >>= (\p -> withPlayerBehavior p (SSAManagementKey saOpen openEyesGID))
    >>= (\p -> withPlayerBehavior p (ISAManagementKey isaLook isaEnabledLookGID))
    >>= (\p -> withPlayerBehavior p (AAManagementKey getRobeAVP getRobeFGID))
    >>= (\p -> withPlayerBehavior p (AVManagementKey get getRobeFGID))

robeObj :: GID DirectionalStimulusActionF  -- Eyes closed look response
        -> GID AcquisitionActionF          -- Can't get (eyes closed/not standing)
        -> WorldDSL Object
robeObj cannotSeeRobeGID cannotGetRobeGID =
  defaultObject & robeObj'
  where
    robeObj' = withShortName "comfortable robe"
                 >=> withDescription "The robe is draped on the chair"
                 >=> withDescriptives [SimpleNounPhrase robeDS]
                 >=> (\o -> withObjectBehavior o (DSAManagementKey look cannotSeeRobeGID))
                 >=> (\o -> withObjectBehavior o (AVManagementKey get cannotGetRobeGID))
                 >=> (\o -> withObjectBehavior o (AAManagementKey getRobeAVP cannotGetRobeGID))

chairObj :: GID DirectionalStimulusActionF
              -> GID AcquisitionActionF
              -> WorldDSL Object
chairObj lookResponseGID getResponseFGID = defaultObject & chairObj'
  where
    chairObj' = withShortName "chair"
                  >=> withDescription "A simple wooden chair"
                  >=> withDescriptives [SimpleNounPhrase chairDS]
                  >=> (\o -> withObjectBehavior o (DSAManagementKey look lookResponseGID))
                  >=> (\o -> withObjectBehavior o (AVManagementKey  get  getResponseFGID))

pocketObj :: GID DirectionalStimulusActionF -> WorldDSL Object
pocketObj lookResponseGID = defaultObject & pocketObj'
  where
    pocketObj' = withShortName "pocket"
                   >=> withDescription "There's something in the pocket"
                   >=> withDescriptives [SimpleNounPhrase pocketDS]
                   >=> (\o -> withObjectBehavior o (DSAManagementKey look lookResponseGID))


placeObject :: GID Location -> GID Object -> DirectionalStimulus -> Objective -> WorldDSL ()
placeObject lid oid ds obj = do
  registerObjectToLocation lid oid (DirectionalStimulusKey ds)
  registerObjectToLocation lid oid (ObjectiveKey obj)

floorObj :: GID DirectionalStimulusActionF -> WorldDSL Object
floorObj lookFloorGID = defaultObject & floorObj'
  where
    floorObj' = withShortName "floor"
                  >=> withDescription "The bedroom floor"
                  >=> withDescriptives [SimpleNounPhrase floorDS]
                  >=> (\o -> withObjectBehavior o (DSAManagementKey look lookFloorGID))
                  -}
