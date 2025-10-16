module Grammar.Parser.Partitions.Prepositions.DirectionalStimulusMarker where

import           Data.HashSet                                (HashSet,
                                                              singleton)
import           Grammar.Parser.Lexer                        (Lexeme (AT, IN))
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Prepositions           (ContainmentMarker (ContainmentMarker),
                                                              DirectionalStimulusMarker (DirectionalStimulusMarker))

makeSemanticValues [| DirectionalStimulusMarker |] [AT]

atDS :: DirectionalStimulusMarker
atDS = DirectionalStimulusMarker AT

inCM :: ContainmentMarker
inCM = ContainmentMarker IN

directionalStimulusMarker :: HashSet DirectionalStimulusMarker
directionalStimulusMarker = singleton atDS

containmentMarker :: HashSet ContainmentMarker
containmentMarker = singleton inCM
