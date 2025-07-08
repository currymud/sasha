module Grammar.Parser.Partitions.Verbs.CardinalMovementVerbs
         (go,sail,sneak,run,march,flee,walk, cardinalMovementVerbs) where

import           Grammar.Parser.Partitions.Templates.Atomics (makeVerbValues)

import           Data.HashSet                                (HashSet, fromList)
import           Model.Parser.Atomics.Verbs                  (CardinalMovementVerb (CardinalMovementVerb))
import           Model.Parser.Lexer                          (Lexeme (FLEE, GO, MARCH, RUN, SAIL, SNEAK, WALK))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Data.Text                                   (Text)
import           Relude.String.Conversion                    (ToText (toText))
import           Test.QuickCheck                             (Arbitrary,
                                                              elements)
import           Test.QuickCheck.Arbitrary                   (Arbitrary (arbitrary))
#endif

makeVerbValues [| CardinalMovementVerb |] [GO, SAIL, SNEAK, RUN, MARCH, FLEE, WALK]

cardinalMovementVerbs :: HashSet CardinalMovementVerb
cardinalMovementVerbs = fromList $ [go,sail,sneak,run,march,flee,walk]

#ifdef TESTING

instance Arbitrary CardinalMovementVerb where
  arbitrary = elements $ HS.toList cardinalMovementVerbs
#endif
