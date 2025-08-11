{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Build.BedPuzzle.Actions.Objects.Pill where
import           Build.Identifiers.Actions                               (notEvenPillGID,
                                                                          takePillDeniedFGID)
import qualified Data.Set
import           Grammar.Parser.Partitions.Adjectives                    (white)
import           Grammar.Parser.Partitions.Misc                          (the)
import           Grammar.Parser.Partitions.Nouns.Consumables             (pill)
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus     (pill)
import           Grammar.Parser.Partitions.Verbs.ConsumptionVerbs        (take)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import           Model.GameState                                         (ActionManagement (CAManagementKey, DSAManagementKey),
                                                                          ActionManagementFunctions (ActionManagementFunctions),
                                                                          Object (Object, _description, _descriptives, _objectActionManagement, _shortName))
import           Model.Parser.Atomics.Nouns                              (Consumable,
                                                                          DirectionalStimulus)
import           Model.Parser.Composites.Nouns                           (ConsumableNounPhrase (ConsumableNounPhrase),
                                                                          DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                                          NounPhrase (DescriptiveNounPhraseDet, SimpleNounPhrase))
import           Model.Parser.Composites.Verbs                           (ConsumptionVerbPhrase (ConsumptionVerbPhrase))
import           Prelude                                                 hiding
                                                                         (take)

pillObj :: Object
pillObj =
  let
  longDescription = DirectionalStimulusNounPhrase (DescriptiveNounPhraseDet the white pillDS)
  shortDescription = DirectionalStimulusNounPhrase (SimpleNounPhrase pillDS)
  in Object
       { _shortName = "pill"
       , _description = "A small, round pill. Probably good for headaches."
       , _descriptives = Data.Set.fromList [longDescription,shortDescription]
       , _objectActionManagement = verbMaps
       }
  where
    verbMaps :: ActionManagementFunctions
    verbMaps = ActionManagementFunctions $ Data.Set.fromList
      [ DSAManagementKey look notEvenPillGID
      , CAManagementKey takePillCVP takePillDeniedFGID
      ]

-- Helper definitions
pillDS :: DirectionalStimulus
pillDS = Grammar.Parser.Partitions.Nouns.DirectionalStimulus.pill

pillConsumable :: Consumable
pillConsumable = Grammar.Parser.Partitions.Nouns.Consumables.pill

takePillCVP :: ConsumptionVerbPhrase
takePillCVP = ConsumptionVerbPhrase take pillConsumableNP

pillConsumableNP :: ConsumableNounPhrase
pillConsumableNP = ConsumableNounPhrase (SimpleNounPhrase pillConsumable)
