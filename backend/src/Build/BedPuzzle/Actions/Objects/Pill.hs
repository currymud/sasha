module Build.BedPuzzle.Actions.Objects.Pill where
import           Build.Identifiers.Actions                               (whatPillGID)
import qualified Data.Map.Strict
import qualified Data.Set
import           Grammar.Parser.Partitions.Adjectives                    (white)
import           Grammar.Parser.Partitions.Misc                          (the)
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus     (pill)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import           Model.GameState                                         (ActionF,
                                                                          Object (Object, _description, _descriptives, _objectActionManagement, _shortName))
import           Model.GID                                               (GID)
import           Model.Parser.Composites.Nouns                           (DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                                          NounPhrase (DescriptiveNounPhraseDet, SimpleNounPhrase))
import           Model.Parser.GCase                                      (VerbKey (DirectionalStimulusKey))

pillObj :: Object
pillObj =
  let
  longDescription = DirectionalStimulusNounPhrase (DescriptiveNounPhraseDet the white pill)
  shortDescription = DirectionalStimulusNounPhrase (SimpleNounPhrase pill)
  in Object
       { _shortName = "pill"
       , _description = "A small, round pill. Probably good for headaches."
       , _descriptives = Data.Set.fromList [longDescription,shortDescription]
       , _objectActionManagement = Data.Map.Strict.fromList verbMaps
       }
  where
    verbMaps :: [(VerbKey, GID ActionF)]
    verbMaps = [(DirectionalStimulusKey look, whatPillGID)]
