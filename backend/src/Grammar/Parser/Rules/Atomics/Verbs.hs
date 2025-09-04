module Grammar.Parser.Rules.Atomics.Verbs (acquisitionVerbRule,
                                           implicitStimulusVerbRule,
                                           positivePosturalVerbRule,
                                           negativePosturalVerbRule,
                                           simpleAccessVerbRule) where
import           Data.Text                                            (Text)
import           Grammar.Parser.Lexer                                 (Lexeme)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs     (acquisitionVerbs)
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb (implicitStimulusVerbs)

import           Grammar.Parser.Partitions.Verbs.PosturalVerbs        (negativePosturalVerbs,
                                                                       positivePosturalVerbs)
import           Grammar.Parser.Partitions.Verbs.SimpleAccessVerbs    (simpleAccessVerbs)
import           Grammar.Parser.Rules.Atomics.Utils                   (parseRule)
import           Model.Parser.Atomics.Verbs                           (AcquisitionVerb (AcquisitionVerb),
                                                                       ImplicitStimulusVerb (ImplicitStimulusVerb),
                                                                       NegativePosturalVerb (NegativePosturalVerb),
                                                                       PositivePosturalVerb (PositivePosturalVerb),
                                                                       SimpleAccessVerb (SimpleAccessVerb))
import           Text.Earley.Grammar                                  (Grammar,
                                                                       Prod)

simpleAccessVerbRule :: Grammar r (Prod r Text Lexeme SimpleAccessVerb)
simpleAccessVerbRule = parseRule simpleAccessVerbs SimpleAccessVerb

implicitStimulusVerbRule :: Grammar r (Prod r Text Lexeme ImplicitStimulusVerb)
implicitStimulusVerbRule = parseRule implicitStimulusVerbs ImplicitStimulusVerb

acquisitionVerbRule :: Grammar r (Prod r Text Lexeme AcquisitionVerb)
acquisitionVerbRule = parseRule acquisitionVerbs AcquisitionVerb

positivePosturalVerbRule :: Grammar r (Prod r Text Lexeme PositivePosturalVerb)
positivePosturalVerbRule = parseRule positivePosturalVerbs PositivePosturalVerb

negativePosturalVerbRule :: Grammar r (Prod r Text Lexeme NegativePosturalVerb)
negativePosturalVerbRule = parseRule negativePosturalVerbs NegativePosturalVerb
