module Parser.PhraseParsers where
import           Data.Text                                (Text)
import           Lexer.Model                              (Lexeme)
import           Parser.SpeechParts                       (parseRule)
import           Parser.SpeechParts.Atomics.Adjectives    (Adjective (Adjective),
                                                           adjectives)
import           Parser.SpeechParts.Atomics.Adverbs       (ImplicitPath (ImplicitPath),
                                                           implicitPaths)
import           Parser.SpeechParts.Composites.Adjectives (AdjPhrase,
                                                           AdjPhraseRules (AdjPhraseRules),
                                                           adjPhraseRule)
import           Text.Earley.Grammar                      (Grammar, Prod)

adjPhraseRule :: Grammar r (Prod r Text Lexeme AdjPhrase)
adjPhraseRule = mdo
  adj <- parseRule adjectives Adjective
  adjSecondary <- parseRule adjectives Adjective
  let adjPhraseRules = AdjPhraseRules adj adjSecondary
  Parser.SpeechParts.Composites.Adjectives.adjPhraseRule adjPhraseRules

implicitPathRule :: Grammar r (Prod r Text Lexeme ImplicitPath)
implicitPathRule = parseRule implicitPaths ImplicitPath
