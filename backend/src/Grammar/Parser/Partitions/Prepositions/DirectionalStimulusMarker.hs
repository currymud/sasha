module Grammar.Parser.Partitions.Prepositions.DirectionalStimulusMarker (directionalStimulusMarker,
                                                                         at) where

import           Data.HashSet                                (HashSet,
                                                              singleton)
import           Grammar.Parser.Lexer                        (Lexeme (AT))
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Prepositions           (DirectionalStimulusMarker (DirectionalStimulusMarker))

makeSemanticValues [| DirectionalStimulusMarker |] [AT, IN]

directionalStimulusMarker :: HashSet DirectionalStimulusMarker
directionalStimulusMarker = singleton at
