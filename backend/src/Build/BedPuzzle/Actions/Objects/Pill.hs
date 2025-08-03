module Build.BedPuzzle.Actions.Objects.Pill where
import           Build.Identifiers.Actions                               (notEvenPillGID)
import           Data.Map.Strict                                         (Map)
import qualified Data.Map.Strict
import qualified Data.Set
import           Grammar.Parser.Partitions.Adjectives                    (white)
import           Grammar.Parser.Partitions.Misc                          (the)
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus     (pill)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import           Model.GameState                                         (ActionManagement (ActionManagement),
                                                                          DirectionalStimulusActionF,
                                                                          ImplicitStimulusActionF,
                                                                          Object (Object, _description, _descriptives, _objectActionManagement, _shortName, _spatialRelationships),
                                                                          SomaticAccessActionF)
import           Model.GID                                               (GID)
import           Model.Parser.Atomics.Verbs                              (DirectionalStimulusVerb,
                                                                          ImplicitStimulusVerb,
                                                                          SomaticAccessVerb)
import           Model.Parser.Composites.Nouns                           (DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                                          NounPhrase (DescriptiveNounPhraseDet, SimpleNounPhrase))

pillObj :: Object
pillObj =
  let
  longDescription = DirectionalStimulusNounPhrase (DescriptiveNounPhraseDet the white pill)
  shortDescription = DirectionalStimulusNounPhrase (SimpleNounPhrase pill)
  in Object
       { _shortName = "pill"
       , _description = "A small, round pill. Probably good for headaches."
       , _descriptives = Data.Set.fromList [longDescription,shortDescription]
       , _objectActionManagement = verbMaps
       , _spatialRelationships = Data.Set.empty
       }
  where
    verbMaps :: ActionManagement
    verbMaps = ActionManagement directionalStimulusVerbs implicitStimulusVerbs somaticAccessVerbs
    implicitStimulusVerbs :: Map ImplicitStimulusVerb (GID ImplicitStimulusActionF)
    implicitStimulusVerbs = Data.Map.Strict.empty
    directionalStimulusVerbs :: Map DirectionalStimulusVerb (GID DirectionalStimulusActionF)
    directionalStimulusVerbs = Data.Map.Strict.fromList [(look, notEvenPillGID :: GID DirectionalStimulusActionF)]
    somaticAccessVerbs :: Map SomaticAccessVerb (GID SomaticAccessActionF)
    somaticAccessVerbs = Data.Map.Strict.empty
  {-
When faced with the possibility Location and Object had different verb case management functions,
I'm opting to keep it the same to manage clarification
for example, player just types the word "pill", game can prompt
"anything special you'd like to do with the pill?" they can now use look in the implicitStimulus case
-}
