module Parser.Model.Nouns where
import           Data.Kind                           (Type)
import           Data.Text                           (Text)
import           Lexer                               (Lexeme)
import           Parser.SpeechParts.Atomics.Nouns    (NamedAgent)
import           Parser.SpeechParts.Composites.Nouns (ContainerPhrase,
                                                      ObjectPhrase,
                                                      SupportPhrase,
                                                      TargetedStimulusNounPhrase)
import           Text.Earley                         (Prod)
import           Text.Earley.Grammar                 (Grammar)

type NounParsers :: (Type -> Type -> Type -> Type) -> Type
data NounParsers r = NounParsers
  { _targetedStimulusNounPhrase' :: Prod r Text Lexeme TargetedStimulusNounPhrase
  , _containerPhrase' :: Prod r Text Lexeme ContainerPhrase
  , _supportPhrase' :: Prod r Text Lexeme SupportPhrase
  , _objectPhrase' :: Prod r Text Lexeme ObjectPhrase
  }
