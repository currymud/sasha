module Parser.SpeechParts where
import           Data.HashSet                          (HashSet, toList)
import           Data.Kind                             (Type)
import           Data.List                             (find)
import           Data.Text                             (Text)
import           Lexer                                 (HasLexeme, Lexeme,
                                                        toLexeme)
import           Parser.SpeechParts.Atomics.Adjectives (Adjective (Adjective),
                                                        adjectives)
import           Parser.SpeechParts.Atomics.Adverbs    (ImplicitPath (..),
                                                        ModToggleAdverb (..),
                                                        implicitPaths,
                                                        modToggleAdverbs)
import           Parser.SpeechParts.Atomics.Misc       (Determiner (Determiner),
                                                        determiners)
import           Parser.SpeechParts.Composites.Verbs   (Imperative, Vocative)
import           Text.Earley                           (rule)
import           Text.Earley.Grammar                   (Grammar, Prod, terminal)

determinerRule :: Grammar r (Prod r Text Lexeme Determiner)
determinerRule = parseRule determiners Determiner

adjRule :: Grammar r (Prod r Text Lexeme Adjective)
adjRule = parseRule adjectives Adjective

modToggleAdverbRule :: Grammar r (Prod r Text Lexeme ModToggleAdverb)
modToggleAdverbRule = parseRule modToggleAdverbs ModToggleAdverb

implicitPathRule :: Grammar r (Prod r Text Lexeme ImplicitPath)
implicitPathRule = parseRule implicitPaths ImplicitPath
  {-
parseRule :: (HasLexeme a) => HashSet a
                                -> (Lexeme -> a)
                                -> Grammar r (Prod r Text Lexeme a)
parseRule morphemes _toMorpheme = rule $ terminal $ \lexeme ->
     find (\m -> toLexeme m == lexeme) (toList morphemes)
-}
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
