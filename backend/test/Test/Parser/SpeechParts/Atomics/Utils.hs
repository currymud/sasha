module Test.Parser.SpeechParts.Atomics.Utils (checkLexeme) where
import           Data.HashSet              (HashSet, toList)
import           Data.Kind                 (Type)
import           Test.QuickCheck           (Testable (property))
import           Test.QuickCheck.Arbitrary (Arbitrary)
import           Test.QuickCheck.Property  (Property)

checkLexeme :: forall (a :: Type).
  ( Arbitrary a, Show a, Eq a) => HashSet a -> Property
checkLexeme lexemes = property checkLexeme'
  where
    xs = toList lexemes
    checkLexeme' ::  a -> Bool
    checkLexeme' lexeme = lexeme `elem` xs

