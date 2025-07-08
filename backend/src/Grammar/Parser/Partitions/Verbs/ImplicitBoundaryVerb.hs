module Grammar.Parser.Partitions.Verbs.ImplicitBoundaryVerb
         (exit,implicitBoundaryVerbs) where

import           Data.HashSet                                (HashSet,
                                                              singleton)
import           Grammar.Parser.Partitions.Templates.Atomics (makeVerbValues)
import           Model.Parser.Atomics.Verbs                  (ImplicitBoundaryVerb (ImplicitBoundaryVerb))
import           Model.Parser.Lexer                          (Lexeme (EXIT))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Data.Text                                   (Text)
import           Relude.String.Conversion                    (ToText (toText))
import           Test.QuickCheck                             (Arbitrary,
                                                              elements)
import           Test.QuickCheck.Arbitrary                   (Arbitrary (arbitrary))
#endif

makeVerbValues [| ImplicitBoundaryVerb |] [EXIT]
implicitBoundaryVerbs :: HashSet ImplicitBoundaryVerb
implicitBoundaryVerbs = singleton exit

#ifdef TESTING

instance Arbitrary ImplicitBoundaryVerb where

  arbitrary = elements $ HS.toList implicitBoundaryVerbs

#endif

