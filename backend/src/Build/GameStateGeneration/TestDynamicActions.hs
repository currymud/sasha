{-# LANGUAGE OverloadedStrings #-}

module Build.GameStateGeneration.TestDynamicActions where

import           Build.GameStateGeneration.Defaults                      (defaultLocation,
                                                                          defaultObject,
                                                                          defaultPlayer)
import qualified Data.Set                                                as Set
import           Model.GameState.GameStateDSL
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
                                                                          robeOB,
                                                                          tableOB)

-- Import adjectives and determiners
import           Grammar.Parser.Partitions.Adjectives                    (small)
import           Grammar.Parser.Partitions.Misc                          (the)
import           Model.Parser.Composites.Nouns                           (ConsumableNounPhrase (ConsumableNounPhrase),
                                                                          DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                                          NounPhrase (DescriptiveNounPhraseDet, SimpleNounPhrase),
                                                                          ObjectPhrase (ObjectPhrase))

-- Import behavior management constructors and spatial relationships
import           Model.GameState                                         (ActionManagement (AAManagementKey, AVManagementKey, CVManagementKey, DSAContainerManagementKey, DSAManagementKey, ISAManagementKey, SSAManagementKey),
                                                                          EffectActionKey (AcquisitionalActionKey, SomaticAccessActionKey),
                                                                          GameState,
                                                                          Location,
                                                                          Object,
                                                                          Player,
                                                                          PlayerKey (PlayerKeyObject),
                                                                          SpatialRelationship (ContainedIn, Contains, SupportedBy, Supports))

-- Import verb functions
import           Grammar.Parser.Partitions.Nouns.Consumables             (pill)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs        (get)
import           Grammar.Parser.Partitions.Verbs.ConsumptionVerbs        (take)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb    (inventory,
                                                                          isaLook)
import           Grammar.Parser.Partitions.Verbs.SomaticAccessVerbs      (saOpen)

-- Import verb phrases
import           Control.Monad                                           ((>=>))
import           Data.Foldable                                           (traverse_)
import           Debug.Trace                                             (trace)
import           Model.Parser.Composites.Verbs                           (AcquisitionVerbPhrase (SimpleAcquisitionVerbPhrase),
                                                                          ConsumptionVerbPhrase (ConsumptionVerbPhrase))
import           Relude.Function                                         ((&))

-- Import action functions from BedPuzzle
import           Build.BedPuzzle.Actions.Open                            (openEyes,
                                                                          openEyesDenied)


testDynamicActionsDSL :: WorldDSL GameState
testDynamicActionsDSL = do
  -- Declare location GID
  bedroomGID <- declareLocationGID (SimpleNounPhrase bedroomDS)

  -- Generate open eyes action GIDs dynamically
  openEyesGID <- declareSomaticActionGID openEyes

  -- Build and register location
  registerLocation bedroomGID (defaultLocation
                                & withTitle "bedroom in bed"
                                & withLocationBehavior  (ISAManagementKey isaLook pitchBlackGID))
  -- Build and register player with the open eyes action
  player <- withPlayerLocation defaultPlayer bedroomGID
              >>= (\p -> withPlayerBehavior p (SSAManagementKey saOpen openEyesGID))
  registerPlayer player

  finalizeGameState
