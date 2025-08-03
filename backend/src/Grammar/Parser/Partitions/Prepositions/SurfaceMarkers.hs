module Grammar.Parser.Partitions.Prepositions.SurfaceMarkers (surfaceMarkers,
                                                             on,
                                                             from) where

import           Data.HashSet                                (HashSet, fromList)
import           Grammar.Parser.Lexer                        (Lexeme (FROM, ON))
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Prepositions           (SurfaceMarker (SurfaceMarker))

makeSemanticValues [| SurfaceMarker|] [ON,FROM]

surfaceMarkers :: HashSet SurfaceMarker
surfaceMarkers = Data.HashSet.fromList [on, from]
