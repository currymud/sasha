module Grammar.Parser.Partitions.Prepositions.ContainmentMarkers (containmentMarkers,in',into) where

import           Data.HashSet                                (HashSet, fromList)
import           Grammar.Parser.Lexer                        (Lexeme (IN, INTO))
import           Grammar.Parser.Partitions.Templates.Atomics (makeLegalSemanticValues,
                                                              makeSemanticValues)
import           Model.Parser.Atomics.Prepositions           (ContainmentMarker (ContainmentMarker))



makeSemanticValues [| ContainmentMarker |] [INTO]

makeLegalSemanticValues [| ContainmentMarker |] [IN]

containmentMarkers :: HashSet ContainmentMarker
containmentMarkers = Data.HashSet.fromList [into, in']
