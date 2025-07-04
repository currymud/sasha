module Grammar.Parser.Rules.Atomics.Utils (parseRule) where
import           Data.HashSet        (HashSet, toList)
import           Data.List           (find)
import           Data.Text           (Text)
import           Grammar.Lexer       (HasLexeme (toLexeme), Lexeme)
import           Text.Earley         (rule)
import           Text.Earley.Grammar (Grammar, Prod, terminal)

parseRule :: (HasLexeme a) => HashSet a
                                -> (Lexeme -> a)
                                -> Grammar r (Prod r Text Lexeme a)
parseRule morphemes toMorpheme = rule $ terminal $ \lexeme ->
     case find (\m -> toLexeme m == lexeme) (toList morphemes) of
       Just _  -> Just (toMorpheme lexeme)
       Nothing -> Nothing

