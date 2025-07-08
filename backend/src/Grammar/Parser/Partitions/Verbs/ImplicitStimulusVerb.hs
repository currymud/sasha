module Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb
         (look,smell,taste,listen,touch,implicitStimulusVerbs) where

import           Data.HashSet                                (HashSet, fromList)
import           Grammar.Parser.Partitions.Templates.Atomics (makeVerbValues)
import           Model.Parser.Atomics.Verbs                  (ImplicitStimulusVerb (ImplicitStimulusVerb))
import           Model.Parser.Lexer                          (Lexeme (LISTEN, LOOK, SMELL, TASTE, TOUCH))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Data.Text                                   (Text)
import           Relude.String.Conversion                    (ToText (toText))
import           Test.QuickCheck                             (Arbitrary,
                                                              elements)
import           Test.QuickCheck.Arbitrary                   (Arbitrary (arbitrary))
#endif

makeVerbValues [| ImplicitStimulusVerb |] [LOOK, SMELL, TASTE,LISTEN, TOUCH]
implicitStimulusVerbs :: HashSet ImplicitStimulusVerb
implicitStimulusVerbs =
  fromList $ [look,smell,taste,listen,touch]

#ifdef TESTING

instance Arbitrary ImplicitStimulusVerb where
  arbitrary = elements $ HS.toList implicitStimulusVerbs

#endif

