{-# LANGUAGE OverloadedStrings #-}

module Build.GameStateGeneration.TestDynamicActions where

import           Build.GameStateGeneration.Defaults                      (defaultLocation,
                                                                          defaultObject,
                                                                          defaultPlayer)
import           Prelude                                                 hiding
                                                                         (take)

-- Import semantic wrappers - DirectionalStimulus versions
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus     (bedroomDS,
                                                                          chairDS)

-- Import adjectives and determiners
import           Model.Parser.Composites.Nouns                           (NounPhrase (SimpleNounPhrase))

-- Import behavior management constructors and spatial relationships
import           Model.GameState                                         (ActionManagement (DSAManagementKey, ISAManagementKey, SSAManagementKey),
                                                                          DirectionalStimulusActionF,
                                                                          EffectActionKey (SomaticAccessActionKey),
                                                                          GameState,
                                                                          ImplicitStimulusActionF,
                                                                          Location,
                                                                          Object,
                                                                          Player,
                                                                          SomaticAccessActionF)
import           Model.GameState.GameStateDSL                            (WorldDSL,
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
                                                                          withDescription,
                                                                          withDescriptives,
                                                                          withLocationBehavior,
                                                                          withObjectBehavior,
                                                                          withPlayerBehavior,
                                                                          withPlayerLocation,
                                                                          withShortName,
                                                                          withTitle)
-- Import verb functions
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb    (isaLook)
import           Grammar.Parser.Partitions.Verbs.SomaticAccessVerbs      (saOpen)

-- Import verb phrases
import           Relude.Function                                         ((&))

-- Import action functions from BedPuzzle
import           Build.BedPuzzle.Actions.Locations.Look                  (lookF,
                                                                          pitchBlackF)
import           Build.BedPuzzle.Actions.Look                            (lookAtF)
import           Build.BedPuzzle.Actions.Open                            (openEyes)
import           Build.BedPuzzle.Actions.Player.Look                     (isvActionEnabled)
import           Control.Monad                                           ((>=>))
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import           Model.GID                                               (GID)
import           Model.Parser.Atomics.Nouns                              (DirectionalStimulus,
                                                                          Objective)
import           Model.Parser.GCase                                      (NounKey (DirectionalStimulusKey, ObjectiveKey))


testDynamicActionsDSL :: WorldDSL GameState
testDynamicActionsDSL = do
  -- Declare location GID
  bedroomGID <- declareLocationGID (SimpleNounPhrase bedroomDS)

-- Object GIDs
  chairGID <- declareObjectGID (SimpleNounPhrase chairDS)
  seeChairGID <- declareDirectionalStimulusActionGID (lookAtF chairGID)

  -- Generate open eyes action GIDs dynamically
  openEyesGID <- declareSomaticActionGID openEyes
  pitchBlackGID <- declareImplicitStimulusActionGID pitchBlackF
  lookFGID <- declareImplicitStimulusActionGID lookF
  isaEnabledLookGID <- declareImplicitStimulusActionGID (isvActionEnabled isaLook) -- Import lookF from Build.BedPuzzle.Actions.Locations.Look
  player <- buildBedroomPlayer bedroomGID isaEnabledLookGID openEyesGID

  openEyesLookChangeEffect <- createImplicitStimulusEffect isaLook lookFGID
  linkEffectToLocation (SomaticAccessActionKey openEyesGID) bedroomGID openEyesLookChangeEffect
  registerPlayer player
  registerLocation bedroomGID (buildLocation pitchBlackGID)
  registerObject chairGID (chairObj seeChairGID)

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
