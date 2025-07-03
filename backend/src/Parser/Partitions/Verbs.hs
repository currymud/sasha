module Parser.Partitions.Verbs where

import           Data.HashSet               (HashSet, fromList, singleton)
import           Model.Lexer                (Lexeme (CLOSE, EXIT, FLEE, GO, LISTEN, LOOK, MARCH, OPEN, RUN, SAIL, SLEEP, SMELL, SNEAK, TASTE, TOUCH, WAIT, WALK))
import           Model.Parser.Atomics.Verbs (CardinalMovementVerb (CardinalMovementVerb),
                                             DirectionalStimulusVerb (DirectionalStimulusVerb),
                                             ImplicitBoundaryVerb (ImplicitBoundaryVerb),
                                             ImplicitRegionalStimulusVerb (ImplicitRegionalStimulusVerb),
                                             ImplicitStimulusVerb (ImplicitStimulusVerb),
                                             SimpleAccessVerb (SimpleAccessVerb))

cardinalMovementVerbs :: HashSet CardinalMovementVerb
cardinalMovementVerbs = fromList
  $ map CardinalMovementVerb [GO, SAIL, SNEAK,RUN , MARCH, FLEE, WALK]

implicitBoundaryVerbs :: HashSet ImplicitBoundaryVerb
implicitBoundaryVerbs = singleton $ ImplicitBoundaryVerb EXIT

implicitStimulusVerbs :: HashSet ImplicitStimulusVerb
implicitStimulusVerbs =
  fromList $ map ImplicitStimulusVerb [LOOK, SMELL, TASTE, LISTEN, TOUCH]

-- Verbs that can take directional prepositions like "at"
directionalStimulusVerbs :: HashSet DirectionalStimulusVerb
directionalStimulusVerbs = singleton $ DirectionalStimulusVerb LOOK

-- These verbs don't take instruments
simpleAccessVerbs :: HashSet SimpleAccessVerb
simpleAccessVerbs = fromList $ map SimpleAccessVerb [OPEN, CLOSE]

implicitRegionalStimulusVerbs :: HashSet ImplicitRegionalStimulusVerb
implicitRegionalStimulusVerbs =
  fromList $ map ImplicitRegionalStimulusVerb [WAIT, SLEEP]
