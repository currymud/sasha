module Grammar.Parser.Partitions.Nouns.Instruments (key,instruments,keyInstrument) where
import           Data.HashSet                                (HashSet)
import qualified Data.HashSet                                as HashSet
import           Grammar.Parser.Lexer                        (Lexeme (KEY))
import           Grammar.Parser.Partitions.Nouns.Utils       (instrumentals')
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Nouns                  (InstrumentalAccessNoun (InstrumentalAccessNoun))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Test.QuickCheck                             (elements)
import           Test.QuickCheck.Arbitrary                   (Arbitrary (arbitrary))
#endif

makeSemanticValues [| InstrumentalAccessNoun |] instrumentals'

instruments :: HashSet InstrumentalAccessNoun
instruments = HashSet.fromList [key]

keyInstrument :: InstrumentalAccessNoun
keyInstrument = key

#ifdef TESTING
instance Arbitrary InstrumentalAccessNoun where
  arbitrary = elements $ HS.toList instruments
#endif
