module Grammar.Parser.Partitions.Prepositions.DirectionalStimulusMarker (directionalStimulusMarker,
                                                                         atDS) where

import           Data.HashSet                                (HashSet,
                                                              singleton)
import           Grammar.Parser.Lexer                        (Lexeme (AT))
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Prepositions           (DirectionalStimulusMarker (DirectionalStimulusMarker))

makeSemanticValues [| DirectionalStimulusMarker |] [AT]

atDS :: DirectionalStimulusMarker
atDS = DirectionalStimulusMarker AT

directionalStimulusMarker :: HashSet DirectionalStimulusMarker
directionalStimulusMarker = singleton at
