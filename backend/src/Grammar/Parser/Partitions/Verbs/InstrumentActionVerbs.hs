module Grammar.Parser.Partitions.Verbs.InstrumentActionVerbs
         (push,instrumentActionVerb) where


import           Data.HashSet                                (HashSet,
                                                              singleton)
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Verbs                  (InstrumentActionVerb (InstrumentActionVerb))
import           Model.Parser.Lexer                          (Lexeme (PUSH))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Data.Text                                   (Text)
import           Relude.String.Conversion                    (ToText (toText))
import           Test.QuickCheck                             (Arbitrary,
                                                              elements)
import           Test.QuickCheck.Arbitrary                   (Arbitrary (arbitrary))
#endif

makeSemanticValues [| InstrumentActionVerb |] [PUSH]

instrumentActionVerb :: HashSet InstrumentActionVerb
instrumentActionVerb = singleton push

#ifdef TESTING

instance Arbitrary InstrumentActionVerb where
  arbitrary = elements instrumentActionVerb

#endif
