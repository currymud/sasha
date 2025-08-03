module Grammar.Parser.Rules.Atomics.Verbs (acquisitionVerbRule, implicitStimulusVerbRule) where
import           Data.Text                                            (Text)
import           Grammar.Parser.Lexer                                 (Lexeme)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs     (acquisitionVerbs)
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb (implicitStimulusVerbs)

import           Grammar.Parser.Rules.Atomics.Utils                   (parseRule)
import           Model.Parser.Atomics.Verbs                           (AcquisitionVerb (AcquisitionVerb),
                                                                       ImplicitStimulusVerb (ImplicitStimulusVerb))
import           Text.Earley.Grammar                                  (Grammar,
                                                                       Prod)

implicitStimulusVerbRule :: Grammar r (Prod r Text Lexeme ImplicitStimulusVerb)
implicitStimulusVerbRule = parseRule implicitStimulusVerbs ImplicitStimulusVerb

acquisitionVerbRule :: Grammar r (Prod r Text Lexeme AcquisitionVerb)
acquisitionVerbRule = parseRule acquisitionVerbs AcquisitionVerb
