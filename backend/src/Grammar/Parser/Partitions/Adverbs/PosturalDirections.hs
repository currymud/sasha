module Grammar.Parser.Partitions.Adverbs.PosturalDirections where

import           Data.HashSet                                (HashSet, fromList)
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Adverbs                (NegativePosturalDirection (NegativePosturalDirection),
                                                              PositivePosturalDirection (PositivePosturalDirection))
import           Model.Parser.Lexer                          (Lexeme (DOWN, UP))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Test.QuickCheck                             (elements)
import           Test.QuickCheck.Arbitrary                   (Arbitrary (arbitrary))
#endif

makeSemanticValues [| PositivePosturalDirection |] [UP]
makeSemanticValues [| NegativePosturalDirection |] [DOWN]

positivePosturalDirections :: HashSet PositivePosturalDirection
positivePosturalDirections = fromList [up]

negativePosturalDirections :: HashSet NegativePosturalDirection
negativePosturalDirections = fromList [down]

#ifdef TESTING

instance Arbitrary PositivePosturalDirection where
  arbitrary = elements $ HS.toList positivePosturalDirections

instance Arbitrary NegativePosturalDirection where
  arbitrary = elements $ HS.toList negativePosturalDirections

#endif
