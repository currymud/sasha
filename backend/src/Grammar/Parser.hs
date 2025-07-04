module Grammar.Parser (
  parseTokens
 , sentenceRules
) where

import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import           Grammar.Lexer                         (Lexeme)
import           Grammar.Model.Parser                  (Sentence (Imperative))
import           Grammar.Parser.Rules.Composites.Verbs (imperativeRules)
import           Relude.String.Conversion              (ToText (toText))
import           Text.Earley.Grammar                   (Grammar, Prod)
import           Text.Earley.Parser                    (fullParses, parser)


parseTokens :: [Lexeme] -> Either Text Sentence
parseTokens toks =
  case sparsed of
    (parsed':_) -> Right parsed'
    []          -> Left ("Nonsense in parsed tokens " <> toks')
    where
      sparsed = fst $ fullParses (parser sentenceRules) toks
      toks' = Text.intercalate " " $ toText <$> toks

sentenceRules :: Grammar r (Prod r Text Lexeme Sentence)
sentenceRules = mdo
  imperative <- imperativeRules
  pure $ Imperative <$> imperative
