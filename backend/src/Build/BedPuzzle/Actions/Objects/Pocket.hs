module Build.BedPuzzle.Actions.Objects.Pocket where
import           Build.Identifiers.Actions                               (notEvenPocketGID)
import           Data.Map.Strict                                         (Map)
import qualified Data.Map.Strict
import qualified Data.Set
import           Grammar.Parser.Partitions.Misc                          (the)
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus     (pocket,
                                                                          table)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import           Model.GameState                                         (AcquisitionActionF,
                                                                          ActionManagement (ActionManagement),
                                                                          DirectionalStimulusActionF,
                                                                          ImplicitStimulusActionF,
                                                                          Object (Object, _description, _descriptives, _objectActionManagement, _shortName),
                                                                          SomaticAccessActionF,
                                                                          SpatialRelationship (ContainedIn))
import           Model.GID                                               (GID)
import           Model.Parser.Atomics.Verbs                              (AcquisitionVerb,
                                                                          DirectionalStimulusVerb,
                                                                          ImplicitStimulusVerb,
                                                                          SomaticAccessVerb)
import           Model.Parser.Composites.Nouns                           (DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                                          NounPhrase (NounPhrase, SimpleNounPhrase))
import           Model.Parser.Composites.Verbs                           (AcquisitionVerbPhrase)

pocketObj :: Object
pocketObj =
  let
  longDescription = DirectionalStimulusNounPhrase (NounPhrase the pocket)
  shortDescription = DirectionalStimulusNounPhrase (SimpleNounPhrase pocket)
  in Object
       { _shortName = "It's a pocket, your robe has a pocket."
       , _description = "It's a ridiculuous pocket that defies physics."
       , _descriptives = Data.Set.fromList [longDescription,shortDescription]
       , _objectActionManagement = verbMaps
       }
  where
    verbMaps :: ActionManagement
    verbMaps = ActionManagement directionalStimulusVerbs implicitStimulusVerbs somaticAccessVerbs acquisitionVerbs
    implicitStimulusVerbs :: Map ImplicitStimulusVerb (GID ImplicitStimulusActionF)
    implicitStimulusVerbs = Data.Map.Strict.empty
    directionalStimulusVerbs :: Map DirectionalStimulusVerb (GID DirectionalStimulusActionF)
    directionalStimulusVerbs =  Data.Map.Strict.fromList [(look, notEvenPocketGID :: GID DirectionalStimulusActionF)]
    somaticAccessVerbs :: Map SomaticAccessVerb (GID SomaticAccessActionF)
    somaticAccessVerbs = Data.Map.Strict.empty
    acquisitionVerbs :: Map AcquisitionVerbPhrase (GID AcquisitionActionF)
    acquisitionVerbs = Data.Map.Strict.empty
