module Grammar.Parser.Partitions.Verbs where

import           Data.HashSet                                (HashSet, fromList,
                                                              singleton)
import           Grammar.Parser.Partitions.Templates.Atomics (makeVerbValues)
import           Model.Parser.Atomics.Verbs                  (CardinalMovementVerb (CardinalMovementVerb),
                                                              DirectionalStimulusVerb (DirectionalStimulusVerb),
                                                              ImplicitBoundaryVerb (ImplicitBoundaryVerb),
                                                              ImplicitRegionalStimulusVerb (ImplicitRegionalStimulusVerb),
                                                              ImplicitStimulusVerb (ImplicitStimulusVerb),
                                                              SimpleAccessVerb (SimpleAccessVerb))
import           Model.Parser.Lexer                          (Lexeme (CLOSE, EXIT, FLEE, GO, LISTEN, LOOK, MARCH, OPEN, RUN, SAIL, SLEEP, SMELL, SNEAK, TASTE, TOUCH, WAIT, WALK))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Data.Text                                   (Text)
import           Relude.String.Conversion                    (ToText (toText))
import           Test.QuickCheck                             (Arbitrary,
                                                              elements)
import           Test.QuickCheck.Arbitrary                   (Arbitrary (arbitrary))
#endif

makeVerbValues
  [| CardinalMovementVerb |]
  [GO, SAIL, SNEAK, RUN, MARCH, FLEE, WALK]

cardinalMovementVerbs :: HashSet CardinalMovementVerb
cardinalMovementVerbs = fromList $ [go,sail,sneak,run,march,flee,walk]

makeVerbValues [| ImplicitBoundaryVerb |] [EXIT]
implicitBoundaryVerbs :: HashSet ImplicitBoundaryVerb
implicitBoundaryVerbs = singleton exit

makeVerbValues [| ImplicitStimulusVerb |] [LOOK, SMELL, TASTE,LISTEN, TOUCH]
implicitStimulusVerbs :: HashSet ImplicitStimulusVerb
implicitStimulusVerbs =
  fromList $ [look,smell,taste,listen,touch]

-- makeVerbValues [| DirectionalStimulusVerb |] [LOOK]
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

