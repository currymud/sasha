module Parser.PhraseParsers where
import           Data.Text                                (Text)
import           Lexer.Model                              (Lexeme)
import           Parser.SpeechParts                       (parseRule)
import           Parser.SpeechParts.Atomics.Adjectives    (Adjective (Adjective),
                                                           adjectives)
import           Parser.SpeechParts.Composites.Adjectives (AdjPhrase,
                                                           AdjPhraseRules (AdjPhraseRules),
                                                           adjPhraseRule)
import           Text.Earley.Grammar                      (Grammar, Prod)

adjectivePhraseParser :: Grammar r (Prod r Text Lexeme AdjPhrase)
adjectivePhraseParser = mdo
  adj <- parseRule adjectives Adjective
  adjSecondary <- parseRule adjectives Adjective
  let adjPhraseRules = AdjPhraseRules adj adjSecondary
  adjPhraseRule adjPhraseRules
