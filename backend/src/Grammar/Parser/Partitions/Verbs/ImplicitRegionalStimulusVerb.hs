module Grammar.Parser.Partitions.Verbs.ImplicitRegionalStimulusVerb
         (wait,sleep,implicitRegionalStimulusVerbs) where

import           Data.HashSet                                (HashSet, fromList)
import           Grammar.Parser.Partitions.Templates.Atomics (makeVerbValues)
import           Model.Parser.Atomics.Verbs                  (ImplicitRegionalStimulusVerb (ImplicitRegionalStimulusVerb))
import           Model.Parser.Lexer                          (Lexeme (SLEEP, WAIT))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Data.Text                                   (Text)
import           Relude.String.Conversion                    (ToText (toText))
import           Test.QuickCheck                             (Arbitrary,
                                                              elements)
import           Test.QuickCheck.Arbitrary                   (Arbitrary (arbitrary))
#endif

makeVerbValues [| ImplicitRegionalStimulusVerb |] [WAIT, SLEEP]

implicitRegionalStimulusVerbs :: HashSet ImplicitRegionalStimulusVerb
implicitRegionalStimulusVerbs =
  fromList [wait,sleep]

#ifdef TESTING

instance Arbitrary ImplicitRegionalStimulusVerb where
  arbitrary = elements $ HS.toList implicitRegionalStimulusVerbs

#endif
