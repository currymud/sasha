module Parser.SpeechParts where
import           Data.HashSet                        (HashSet, toList)
import           Data.Kind                           (Type)
import           Data.List                           (find)
import           Data.Text                           (Text)
import           Lexer                               (HasLexeme, Lexeme,
                                                      toLexeme)
import           Parser.SpeechParts.Composites.Verbs (Imperative, Vocative)
import           Text.Earley                         (rule)
import           Text.Earley.Grammar                 (Grammar, Prod, terminal)

parseRule :: (HasLexeme a) => HashSet a
                                -> (Lexeme -> a)
                                -> Grammar r (Prod r Text Lexeme a)
parseRule morphemes toMorpheme = rule $ terminal $ \lexeme ->
     case find (\m -> toLexeme m == lexeme) (toList morphemes) of
       Just _  -> Just (toMorpheme lexeme)
       Nothing -> Nothing

type Sentence :: Type
data Sentence
  = Nominative Imperative
  | Vocative Vocative
  deriving stock (Eq,Ord,Show)
