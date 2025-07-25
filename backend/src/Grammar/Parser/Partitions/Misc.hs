module Grammar.Parser.Partitions.Misc where
import           Data.HashSet                                (HashSet, fromList)
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Misc                   (Determiner (Determiner))
import           Model.Parser.Lexer                          (Lexeme (A, THE))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Data.Text                                   (Text)
import           Relude.String.Conversion                    (ToText (toText))
import           Test.QuickCheck                             (Arbitrary,
                                                              elements)
import           Test.QuickCheck.Arbitrary                   (Arbitrary (arbitrary))
#endif

makeSemanticValues [| Determiner |] [THE,A]

determiners :: HashSet Determiner
determiners = fromList [a,the]

#ifdef TESTING
instance Arbitrary Determiner where
  arbitrary = elements [a,the]
#endif
