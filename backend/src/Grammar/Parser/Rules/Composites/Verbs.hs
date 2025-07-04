module Grammar.Parser.Rules.Composites.Verbs
  (stimulusVerbPhraseRules
  , imperativeRules
  ) where
import           Data.Text                             (Text)
import           Grammar.Lexer                         (Lexeme)
import           Grammar.Model.Parser.Composites.Verbs (Imperative (StimulusVerbPhrase),
                                                        StimulusVerbPhrase (ImplicitStimulusVerb))
import           Grammar.Parser.Rules.Atomics.Verbs    (implicitStimulusVerbRule)
import           Text.Earley.Grammar                   (Grammar, Prod, rule)

stimulusVerbPhraseRules :: Grammar r (Prod r Text Lexeme StimulusVerbPhrase)
stimulusVerbPhraseRules = do
  implicitStimulusVerbRule' <- implicitStimulusVerbRule
  rule $ ImplicitStimulusVerb <$> implicitStimulusVerbRule'

imperativeRules :: Grammar r (Prod r Text Lexeme Imperative)
imperativeRules = do
  stimulusVerbPhrase <- stimulusVerbPhraseRules
  rule $ StimulusVerbPhrase <$> stimulusVerbPhrase
