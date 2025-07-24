module Grammar.Parser.Partitions.Verbs.SimpleAccessVerbs where

import           Data.HashSet               (HashSet, fromList)
import           Model.Parser.Atomics.Verbs (SimpleAccessVerb (SimpleAccessVerb))
import           Model.Parser.Lexer         (Lexeme (CLOSE, OPEN))

#ifdef TESTING
import qualified Data.HashSet               as HS
import           Data.Text                  (Text)
import           Relude.String.Conversion   (ToText (toText))
import           Test.QuickCheck            (Arbitrary, elements)
import           Test.QuickCheck.Arbitrary  (Arbitrary (arbitrary))
#endif

-- These verbs don't take instruments
simpleAccessVerbs :: HashSet SimpleAccessVerb
simpleAccessVerbs = fromList $ map SimpleAccessVerb [OPEN, CLOSE]

#ifdef TESTING

instance Arbitrary SimpleAccessVerb where
  arbitrary = elements $ HS.toList simpleAccessVerbs

#endif

