module Parser.Partitions.Adverbs (implicitPaths) where
import           Data.HashSet                 (HashSet, fromList)
import           Lexer                        (Lexeme (DOWN, EAST, LEFT, NORTH, RIGHT, SOUTH, UP, WEST))
import           Model.Parser.Atomics.Adverbs (ImplicitPath (ImplicitPath))

#ifdef TESTING
import qualified Data.HashSet                 as HS
import           Relude.String.Conversion     (ToText (toText))
import           Test.QuickCheck              (Arbitrary, elements)
import           Test.QuickCheck.Arbitrary    (Arbitrary (arbitrary))
#endif

implicitPaths :: HashSet ImplicitPath
implicitPaths =
  fromList $ map ImplicitPath [NORTH, EAST, SOUTH, WEST, UP, DOWN, LEFT,RIGHT]

#ifdef TESTING

instance Arbitrary ImplicitPath where
  arbitrary = elements $ HS.toList implicitPaths

#endif
