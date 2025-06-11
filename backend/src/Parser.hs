module Parser (
  module Text.Earley
, module Parser
) where

import           Control.Applicative             ((<|>))
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Lexer
import           Parser.NounParsers              (nounParsers)
import           Parser.PhraseParsers            (adjectivePhraseParser)
import           Parser.SpeechParts              (Sentence (..), parseRule)
import           Parser.SpeechParts.Atomics.Misc (Determiner (..), determiners)
import           Parser.VerbParsers              (imperativePhraseParser,
                                                  vocativeParser)
import           Relude.String.Conversion        (ToText (toText))
import           Text.Earley

parseTokens :: [Lexeme] -> Either Text Sentence
parseTokens toks =
  case sparsed of
    (parsed':_) -> Right parsed'
    []          -> Left ("Nonsense in parsed tokens " <> toks')
    where
      sparsed = fst $ fullParses (parser sentenceParser) toks
      toks' = Text.intercalate " " $ toText <$> toks

sentenceParser :: Grammar r (Prod r Text Lexeme Sentence)
sentenceParser = mdo
  determiner <- parseRule determiners Determiner
  adjPhrase <- adjectivePhraseParser
  nounParsers' <- nounParsers determiner adjPhrase
  imperative <- imperativePhraseParser nounParsers' determiner adjPhrase
  vocative <- vocativeParser imperative nounParsers'
  pure $ Nominative <$> imperative
           <|> Vocative <$> vocative

