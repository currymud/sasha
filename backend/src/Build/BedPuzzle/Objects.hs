module Build.BedPuzzle.Objects where
import           Build.Identifiers.Actions                            (pitchBlackFGID)
import qualified Data.Map.Strict
import qualified Data.Set
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb (look)
import           Model.GameState                                      (Object (Object),
                                                                       _description,
                                                                       _descriptives,
                                                                       _objectActionManagement,
                                                                       _shortName)
import           Model.Parser.GCase                                   (VerbKey (ImplicitStimulusKey))

