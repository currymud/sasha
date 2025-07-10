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

playerOBJ :: Object
playerOBJ = Object
  { _shortName = "test player"
  , _description = "A test player object for the game."
  , _descriptives = Data.Set.empty
  , _objectActionManagement = Data.Map.Strict.fromList
      [ (ImplicitStimulusKey look, pitchBlackFGID)]
  }
