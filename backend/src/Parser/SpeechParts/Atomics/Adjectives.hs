module Parser.SpeechParts.Atomics.Adjectives where

import           Data.Hashable             (Hashable)
import           Data.HashSet              (HashSet, fromList)
import           Data.Kind                 (Type)
import           Lexer

#ifdef TESTING
import qualified Data.HashSet              as HS
import           Relude.String.Conversion  (ToText (toText))
import           Test.QuickCheck           (Arbitrary, elements)
import           Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
#endif


type Adjective :: Type
newtype Adjective =
  Adjective { _fromAdjective :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme Adjective where
  toLexeme = _fromAdjective

adjectives :: HashSet Adjective
adjectives = fromList $ map Adjective adjlex
  where adjlex = [MIND, BLUE, RED, GREAT, LONG, OLD, DRUNK, PLANT, POT, TEA
                 , CABINET, LOCKED, UNLOCKED, KITCHEN, LEFT, RIGHT
                 , FRONT, BEHIND, SMALL, LARGE]
#ifdef TESTING
instance Arbitrary Adjective where
  arbitrary = elements $ HS.toList adjectives
#endif
