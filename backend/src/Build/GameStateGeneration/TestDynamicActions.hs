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
                                                                       GameState)
import           Model.GameState.GameStateDSL                         (WorldDSL,
                                                                       declareImplicitStimulusActionGID,
                                                                       declareLocationGID,
                                                                       declareSomaticActionGID,
                                                                       finalizeGameState,
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
import           Build.BedPuzzle.Actions.Locations.Look               (pitchBlackF)
import           Build.BedPuzzle.Actions.Open                         (openEyes)
import           Control.Monad                                        ((>=>))


testDynamicActionsDSL :: WorldDSL GameState
testDynamicActionsDSL = do
  -- Declare location GID
  bedroomGID <- declareLocationGID (SimpleNounPhrase bedroomDS)

  -- Generate open eyes action GIDs dynamically
  openEyesGID <- declareSomaticActionGID openEyes
  pitchBlackGID <- declareImplicitStimulusActionGID pitchBlackF

  let bedroomLoc' loc = withTitle "bedroom in bed" loc
                        >>= \l -> withLocationBehavior l (ISAManagementKey isaLook pitchBlackGID)
  registerLocation bedroomGID (bedroomLoc' defaultLocation)

  -- Build and register location
    {-
  registerLocation bedroomGID (defaultLocation
                                & withTitle "bedroom in bed"
                                >=> (\o -> withLocationBehavior o (ISAManagementKey isaLook pitchBlackGID)))
                                -}
  -- Build and register player with the open eyes action
    {-
  player <- withPlayerLocation defaultPlayer bedroomGID
              >>= (\p -> withPlayerBehavior p (SSAManagementKey saOpen openEyesGID))
  registerPlayer player
-}
  finalizeGameState
