module Build.GameStateGeneration.LocationSpec.Locations where

import           Build.GameStateGeneration.LocationSpec.LocationGIDs  (bedroomGID,
                                                                       bedroomUpstairsGID)
import           Build.GameStateGeneration.ObjectSpec                 (defaultLocation,
                                                                       withLocationBehaviors,
                                                                       withObjects,
                                                                       withTitle)
import           Build.GameStateGeneration.ObjectSpec.ObjectGIDS      (chairGID,
                                                                       smallTableGID)
import           Build.Identifiers.Actions                            (pitchBlackFGID)
import           Build.Templates.Identification                       (makeLocationMap)
import           Data.Function                                        ((&))
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus  (chair,
                                                                       table)
import qualified Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb (look)
import           Model.GameState                                      (ActionManagement (ISAManagementKey),
                                                                       Location)
import           Model.Parser.GCase                                   (NounKey (DirectionalStimulusKey, ObjectiveKey))

import           Build.GameStateGeneration.ObjectSpec.ObjectGIDS      (chairGID,
                                                                       floorGID,
                                                                       mailGID,
                                                                       robeGID,
                                                                       smallTableGID)
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus  (chair,
                                                                       floor,
                                                                       mail,
                                                                       robe,
                                                                       table)
import qualified Grammar.Parser.Partitions.Nouns.Objectives           as Objectives
import           Prelude                                              hiding
                                                                      (floor)
-- Create locations using builder pattern
bedroom :: Location
bedroom = defaultLocation
  & withTitle "Bedroom in Bed"
  & withObjects [ (chairGID, DirectionalStimulusKey chair)
                , (smallTableGID, DirectionalStimulusKey table)
                , (robeGID, DirectionalStimulusKey robe)
                , (mailGID, DirectionalStimulusKey mail)
                , (floorGID, DirectionalStimulusKey floor)
                , (chairGID, ObjectiveKey Objectives.chair)
                , (smallTableGID, ObjectiveKey Objectives.table)
                , (robeGID, ObjectiveKey Objectives.robe)
                , (mailGID, ObjectiveKey Objectives.mail)
                , (floorGID, ObjectiveKey Objectives.floor)
                ]
  & withLocationBehaviors [ISAManagementKey Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb.look pitchBlackFGID]
bedroomUpstairs :: Location
bedroomUpstairs = defaultLocation
  & withTitle "Upstairs Bedroom"
  & withObjects [ (chairGID, DirectionalStimulusKey chair) ]
  & withLocationBehaviors [ISAManagementKey Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb.look pitchBlackFGID]

-- Generate the location map using Template Haskell
makeLocationMap [ ('bedroomGID, 'bedroom)
                , ('bedroomUpstairsGID, 'bedroomUpstairs)
                ]

-- This generates:
-- locationMap :: GIDToDataMap Location Location
-- locationMap = GIDToDataMap $ Data.Map.Strict.fromList
--   [ (bedroomGID, bedroom)
--   , (bedroomUpstairsGID, bedroomUpstairs)
--   ]
