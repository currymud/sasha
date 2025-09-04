module Grammar.Parser.Partitions.Prepositions.InstrumentMarkers (with,instrumentMarkers
                                                             ) where

import           Data.HashSet                                (HashSet, fromList)
import           Grammar.Parser.Lexer                        (Lexeme (WITH))
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Prepositions           (InstrumentMarker (InstrumentMarker))

makeSemanticValues [| InstrumentMarker|] [WITH]

instrumentMarkers :: HashSet InstrumentMarker
instrumentMarkers = Data.HashSet.fromList [with]
