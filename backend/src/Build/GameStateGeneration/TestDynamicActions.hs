{-# LANGUAGE OverloadedStrings #-}

module Build.GameStateGeneration.TestDynamicActions where

import           Build.GameStateGeneration.Defaults                               (defaultLocation,
                                                                                   defaultObject,
                                                                                   defaultPlayer)
import           Prelude                                                          hiding
                                                                                  (take)

-- Import semantic wrappers - DirectionalStimulus versions
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus              (bedroomDS,
                                                                                   chairDS,
                                                                                   floorDS)

-- Import adjectives and determiners
import           Model.Parser.Composites.Nouns                                    (DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                                                   NounPhrase (SimpleNounPhrase))

-- Import behavior management constructors and spatial relationships
import           Model.GameState                                                  (ActionManagement (DSAManagementKey, ISAManagementKey, SSAManagementKey),
                                                                                   DirectionalStimulusActionF,
                                                                                   EffectActionKey (SomaticAccessActionKey),
                                                                                   GameState,
                                                                                   ImplicitStimulusActionF,
                                                                                   Location,
                                                                                   Object,
                                                                                   Player,
                                                                                   SomaticAccessActionF,
                                                                                   SpatialRelationship (SupportedBy, Supports))
import           Model.GameState.GameStateDSL                                     (WorldDSL,
                                                                                   createImplicitStimulusEffect,
                                                                                   declareDirectionalStimulusActionGID,
                                                                                   declareImplicitStimulusActionGID,
                                                                                   declareLocationGID,
                                                                                   declareObjectGID,
                                                                                   declareSomaticActionGID,
                                                                                   finalizeGameState,
                                                                                   linkEffectToLocation,
                                                                                   registerLocation,
                                                                                   registerObject,
                                                                                   registerObjectToLocation,
                                                                                   registerPlayer,
                                                                                   registerSpatial,
                                                                                   setPerceptionMap,
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
import           Build.BedPuzzle.Actions.Locations.Look                           (lookF,
                                                                                   pitchBlackF)
import           Build.BedPuzzle.Actions.Look                                     (lookAtF)
import           Build.BedPuzzle.Actions.Open                                     (openEyes)
import           Build.BedPuzzle.Actions.Player.Look                              (isvActionEnabled)
import           Control.Monad                                                    ((>=>))
import qualified Data.Set
import           Grammar.Parser.Partitions.Nouns.Objectives                       (chairOB,
                                                                                   floorOB)
import           Grammar.Parser.Partitions.Prepositions.DirectionalStimulusMarker (at)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb          (look)
import           Model.GID                                                        (GID)
import           Model.Parser.Atomics.Nouns                                       (DirectionalStimulus,
                                                                                   Objective)
import           Model.Parser.GCase                                               (NounKey (DirectionalStimulusKey, ObjectiveKey))


testDynamicActionsDSL :: WorldDSL GameState
testDynamicActionsDSL = do
  -- Declare location GID
  bedroomGID <- declareLocationGID (SimpleNounPhrase bedroomDS)

-- Object GIDs
  floorGID <- declareObjectGID (SimpleNounPhrase floorDS)
  chairGID <- declareObjectGID (SimpleNounPhrase chairDS)
  seeChairGID <- declareDirectionalStimulusActionGID (lookAtF chairGID)
  lookFloorGID <- declareDirectionalStimulusActionGID (lookAtF floorGID)
  registerObject chairGID (chairObj seeChairGID)
  registerObject floorGID (floorObj lookFloorGID)

  -- Generate open eyes action GIDs dynamically
  openEyesGID <- declareSomaticActionGID openEyes
  pitchBlackGID <- declareImplicitStimulusActionGID pitchBlackF
  lookFGID <- declareImplicitStimulusActionGID lookF
  isaEnabledLookGID <- declareImplicitStimulusActionGID (isvActionEnabled isaLook)

  placeObject bedroomGID chairGID chairDS chairOB
  placeObject bedroomGID floorGID floorDS floorOB

  registerSpatial chairGID (SupportedBy floorGID)
  registerSpatial floorGID (Supports (Data.Set.singleton chairGID))

--  registerObject floorGID (floorObj id)
  player <- buildBedroomPlayer bedroomGID isaEnabledLookGID openEyesGID

  openEyesLookChangeEffect <- createImplicitStimulusEffect isaLook lookFGID
  linkEffectToLocation (SomaticAccessActionKey openEyesGID) bedroomGID openEyesLookChangeEffect
  registerPlayer player
  registerLocation bedroomGID (buildLocation pitchBlackGID)
  setPerceptionMap
    [ (DirectionalStimulusNounPhrase at (SimpleNounPhrase chairDS), [chairGID])
--    , (DirectionalStimulusNounPhrase (SimpleNounPhrase tableDS), [tableGID])
--    , (DirectionalStimulusNounPhrase (SimpleNounPhrase robeDS), [robeGID])
--    , (DirectionalStimulusNounPhrase (SimpleNounPhrase mailDS), [mailGID])
    , (DirectionalStimulusNounPhrase at (SimpleNounPhrase floorDS), [floorGID])
    ]

  finalizeGameState

buildLocation :: GID ImplicitStimulusActionF -> WorldDSL Location
buildLocation pitchBlackGID = defaultLocation & bedroomLoc'
  where
    bedroomLoc' = withTitle "bedroom in bed"
                    >=> (\l -> withLocationBehavior l (ISAManagementKey isaLook pitchBlackGID))

buildBedroomPlayer :: GID Location -> GID ImplicitStimulusActionF -> GID SomaticAccessActionF -> WorldDSL Player
buildBedroomPlayer bedroomGID isaEnabledLookGID openEyesGID =
  withPlayerLocation defaultPlayer bedroomGID
    >>= (\p -> withPlayerBehavior p (ISAManagementKey isaLook isaEnabledLookGID))
    >>= (\p -> withPlayerBehavior p (SSAManagementKey saOpen openEyesGID))

chairObj :: GID DirectionalStimulusActionF -> WorldDSL Object
chairObj seeChairGID = defaultObject & chairObj'
  where
    chairObj' = withShortName "chair"
                  >=> withDescription "A simple wooden chair"
                  >=> withDescriptives [SimpleNounPhrase chairDS]
                  >=> (\o -> withObjectBehavior o (DSAManagementKey look seeChairGID))

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
