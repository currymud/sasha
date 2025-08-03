module Grammar.Parser.Partitions.Prepositions.SourceMarkers (sourceMarkers,
                                                             from) where

import           Data.HashSet                                (HashSet, fromList)
import           Grammar.Parser.Lexer                        (Lexeme (FROM))
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Prepositions           (SourceMarker (SourceMarker))

makeSemanticValues [| SourceMarker|] [FROM]

sourceMarkers :: HashSet SourceMarker
sourceMarkers = Data.HashSet.fromList [from]
