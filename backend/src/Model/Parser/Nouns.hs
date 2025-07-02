module Model.Parser.Nouns where
import           Data.Kind                     (Type)
import           Data.Text                     (Text)
import           Lexer                         (Lexeme)
import           Parser.Rules.Atomics.Nouns    (NamedAgent)
import           Parser.Rules.Composites.Nouns (ContainerPhrase, ObjectPhrase,
                                                SupportPhrase,
                                                TargetedStimulusNounPhrase)
import           Text.Earley                   (Prod)
import           Text.Earley.Grammar           (Grammar)

type NounRules :: (Type -> Type -> Type -> Type) -> Type
data NounRules r = NounRules
  { _targetedStimulusNounPhrase' :: Prod r Text Lexeme TargetedStimulusNounPhrase
  , _containerPhrase' :: Prod r Text Lexeme ContainerPhrase
  , _supportPhrase' :: Prod r Text Lexeme SupportPhrase
  , _objectPhrase' :: Prod r Text Lexeme ObjectPhrase
  }
