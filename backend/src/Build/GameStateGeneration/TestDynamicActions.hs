{-# LANGUAGE OverloadedStrings #-}

module Build.GameStateGeneration.TestDynamicActions where

import           Build.GameStateGeneration.Defaults                   (defaultLocation,
                                                                       defaultPlayer)
import           Prelude                                              hiding
                                                                      (take)

-- Import semantic wrappers - DirectionalStimulus versions
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus  (bedroomDS)

-- Import adjectives and determiners
import           Model.Parser.Composites.Nouns                        (NounPhrase (SimpleNounPhrase))

-- Import behavior management constructors and spatial relationships
import           Model.GameState                                      (ActionManagement (ISAManagementKey, SSAManagementKey),
                                                                       EffectActionKey (SomaticAccessActionKey),
                                                                       GameState,
                                                                       ImplicitStimulusActionF,
                                                                       Location,
                                                                       Player,
                                                                       SomaticAccessActionF)
import           Model.GameState.GameStateDSL                         (WorldDSL,
                                                                       createImplicitStimulusEffect,
                                                                       declareImplicitStimulusActionGID,
                                                                       declareLocationGID,
                                                                       declareSomaticActionGID,
                                                                       finalizeGameState,
                                                                       linkEffectToLocation,
                                                                       registerLocation,
                                                                       registerPlayer,
                                                                       withLocationBehavior,
                                                                       withPlayerBehavior,
                                                                       withPlayerLocation,
                                                                       withTitle)
-- Import verb functions
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb (isaLook)
import           Grammar.Parser.Partitions.Verbs.SomaticAccessVerbs   (saOpen)

-- Import verb phrases
import           Relude.Function                                      ((&))

-- Import action functions from BedPuzzle
import           Build.BedPuzzle.Actions.Locations.Look               (lookF,
                                                                       pitchBlackF)
import           Build.BedPuzzle.Actions.Open                         (openEyes)
import           Build.BedPuzzle.Actions.Player.Look                  (isvActionEnabled)
import           Control.Monad                                        ((>=>))
import           Model.GID                                            (GID)


testDynamicActionsDSL :: WorldDSL GameState
testDynamicActionsDSL = do
  -- Declare location GID
  bedroomGID <- declareLocationGID (SimpleNounPhrase bedroomDS)

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
