module Build.BedPuzzle.Actions.Objects.Table where
import           Build.Identifiers.Actions                               (whatPillGID,
                                                                          whatTableGID)
import           Data.Map.Strict                                         (Map)
import qualified Data.Map.Strict
import qualified Data.Set
import           Grammar.Parser.Partitions.Adjectives                    (small,
                                                                          white)
import           Grammar.Parser.Partitions.Misc                          (the)
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus     (pill,
                                                                          table)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import           Model.GameState                                         (ActionManagement (ActionManagement),
                                                                          DirectionalStimulusActionF,
                                                                          ImplicitStimulusActionF,
                                                                          Object (Object, _description, _descriptives, _objectActionManagement, _shortName),
                                                                          SomaticAccessActionF)
import           Model.GID                                               (GID)
import           Model.Parser.Atomics.Verbs                              (DirectionalStimulusVerb,
                                                                          ImplicitStimulusVerb,
                                                                          SomaticAccessVerb)
import           Model.Parser.Composites.Nouns                           (DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                                          NounPhrase (DescriptiveNounPhraseDet, SimpleNounPhrase))

tableObj :: Object
tableObj =
  let
  longDescription = DirectionalStimulusNounPhrase (DescriptiveNounPhraseDet the small table)
  shortDescription = DirectionalStimulusNounPhrase (SimpleNounPhrase table)
  in Object
       { _shortName = "table"
       , _description = "It's the table next to your bed."
       , _descriptives = Data.Set.fromList [longDescription,shortDescription]
       , _objectActionManagement = verbMaps
       }
  where
    verbMaps :: ActionManagement
    verbMaps = ActionManagement directionalStimulusVerbs implicitStimulusVerbs somaticAccessVerbs
    implicitStimulusVerbs :: Map ImplicitStimulusVerb (GID ImplicitStimulusActionF)
    implicitStimulusVerbs = Data.Map.Strict.empty
    directionalStimulusVerbs :: Map DirectionalStimulusVerb (GID DirectionalStimulusActionF)
    directionalStimulusVerbs =  Data.Map.Strict.fromList [(look, whatTableGID :: GID DirectionalStimulusActionF)]
    somaticAccessVerbs :: Map SomaticAccessVerb (GID SomaticAccessActionF)
    somaticAccessVerbs = Data.Map.Strict.empty
