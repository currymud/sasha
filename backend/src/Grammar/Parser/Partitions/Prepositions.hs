module Grammar.Parser.Partitions.Prepositions where

import           Data.HashSet                                (HashSet, fromList,
                                                              singleton)
import           Grammar.Parser.Lexer                        (Lexeme (AT, FROM, IN, INTO, ON))
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Prepositions           (ContainmentMarker (ContainmentMarker),
                                                              DirectionalStimulusMarker (DirectionalStimulusMarker),
                                                              SurfaceMarker (SurfaceMarker))

makeSemanticValues [| DirectionalStimulusMarker |] [AT]

makeSemanticValues [| SurfaceMarker|] [ON,FROM]

makeSemanticValues [| ContainmentMarker |] [IN,INTO]
directionalStimulusMarker :: HashSet DirectionalStimulusMarker
directionalStimulusMarker = singleton at

surfaceMarkers :: HashSet SurfaceMarker
surfaceMarkers = Data.HashSet.fromList [on, from]
