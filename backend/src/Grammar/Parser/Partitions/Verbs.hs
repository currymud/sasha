module Grammar.Parser.Partitions.Verbs where

import           Data.HashSet                       (HashSet, fromList,
                                                     singleton)
import           Grammar.Model.Lexer                (Lexeme (CLOSE, EXIT, FLEE, GO, LISTEN, LOOK, MARCH, OPEN, RUN, SAIL, SLEEP, SMELL, SNEAK, TASTE, TOUCH, WAIT, WALK))
import           Grammar.Model.Parser.Atomics.Verbs (CardinalMovementVerb (CardinalMovementVerb),
                                                     DirectionalStimulusVerb (DirectionalStimulusVerb),
                                                     ImplicitBoundaryVerb (ImplicitBoundaryVerb),
                                                     ImplicitRegionalStimulusVerb (ImplicitRegionalStimulusVerb),
                                                     ImplicitStimulusVerb (ImplicitStimulusVerb),
                                                     SimpleAccessVerb (SimpleAccessVerb))

#ifdef TESTING
import qualified Data.HashSet                       as HS
import           Data.Text                          (Text)
import           Relude.String.Conversion           (ToText (toText))
import           Test.QuickCheck                    (Arbitrary, elements)
import           Test.QuickCheck.Arbitrary          (Arbitrary (arbitrary))
#endif

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

#ifdef TESTING

instance Arbitrary CardinalMovementVerb where
  arbitrary = elements $ HS.toList cardinalMovementVerbs

instance Arbitrary ImplicitBoundaryVerb where
  arbitrary = elements $ HS.toList implicitBoundaryVerbs

instance Arbitrary ImplicitStimulusVerb where
  arbitrary = elements $ HS.toList implicitStimulusVerbs

instance Arbitrary DirectionalStimulusVerb where
  arbitrary = elements $ HS.toList directionalStimulusVerbs

instance Arbitrary SimpleAccessVerb where
  arbitrary = elements $ HS.toList simpleAccessVerbs

#endif

