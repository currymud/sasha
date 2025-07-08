module Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb where

import           Data.HashSet                                (HashSet,
                                                              singleton)
import           Grammar.Parser.Partitions.Templates.Atomics (makeVerbValues)
import           Model.Parser.Atomics.Verbs                  (DirectionalStimulusVerb (DirectionalStimulusVerb))
import           Model.Parser.Lexer                          (Lexeme (LOOK))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Data.Text                                   (Text)
import           Relude.String.Conversion                    (ToText (toText))
import           Test.QuickCheck                             (Arbitrary,
                                                              elements)
import           Test.QuickCheck.Arbitrary                   (Arbitrary (arbitrary))
#endif

makeVerbValues [| DirectionalStimulusVerb |] [LOOK]
-- Verbs that can take directional prepositions like "at"
directionalStimulusVerbs :: HashSet DirectionalStimulusVerb
directionalStimulusVerbs = singleton look

#ifdef TESTING

instance Arbitrary DirectionalStimulusVerb where
  arbitrary = elements $ HS.toList directionalStimulusVerbs

#endif

