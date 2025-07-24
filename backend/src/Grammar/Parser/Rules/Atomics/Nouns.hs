module Grammar.Parser.Rules.Atomics.Nouns (DirectionalStimulusNounRule (DirectionalStimulusNounRule, _directionalStimulusRule)) where
import           Data.Kind                  (Type)
import           Data.Text                  (Text)
import           Model.Parser.Atomics.Nouns (DirectionalStimulus)
import           Model.Parser.Lexer         (Lexeme)
import           Text.Earley                (Prod)

type DirectionalStimulusNounRule :: (Type -> Type -> Type -> Type) -> Type
newtype DirectionalStimulusNounRule r = DirectionalStimulusNounRule
  { _directionalStimulusRule :: Prod r Text Lexeme DirectionalStimulus}
