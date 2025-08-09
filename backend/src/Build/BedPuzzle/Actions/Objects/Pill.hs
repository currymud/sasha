module Build.BedPuzzle.Actions.Objects.Pill where
import           Build.Identifiers.Actions                               (notEvenPillGID)
import           Data.Map.Strict                                         (Map)
import qualified Data.Map.Strict
import qualified Data.Set
import           Grammar.Parser.Partitions.Adjectives                    (white)
import           Grammar.Parser.Partitions.Misc                          (the)
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus     (pill)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import           Model.GameState                                         (AcquisitionActionF,
                                                                          ActionManagement (DSAManagementKey),
                                                                          ActionManagementFunctions (ActionManagementFunctions),
                                                                          ConsumptionActionF,
                                                                          DirectionalStimulusActionF,
                                                                          ImplicitStimulusActionF,
                                                                          Object (Object, _description, _descriptives, _objectActionManagement, _shortName),
                                                                          SomaticAccessActionF)
import           Model.GID                                               (GID)
import           Model.Parser.Atomics.Verbs                              (AcquisitionVerb,
                                                                          DirectionalStimulusVerb,
                                                                          ImplicitStimulusVerb,
                                                                          SomaticAccessVerb)
import           Model.Parser.Composites.Nouns                           (DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                                          NounPhrase (DescriptiveNounPhraseDet, SimpleNounPhrase))
import           Model.Parser.Composites.Verbs                           (AcquisitionVerbPhrase,
                                                                          ConsumptionVerbPhrase)

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
       }
  where
    verbMaps :: ActionManagementFunctions
    verbMaps = directionalStimulusVerbs
    directionalStimulusVerbs :: ActionManagementFunctions
    directionalStimulusVerbs = ActionManagementFunctions $ Data.Set.singleton (DSAManagementKey look notEvenPillGID)
  {-
When faced with the possibility Location and Object had different verb case management functions,
I'm opting to keep it the same to manage clarification
for example, player just types the word "pill", game can prompt
"anything special you'd like to do with the pill?" they can now use look in the implicitStimulus case
-}
