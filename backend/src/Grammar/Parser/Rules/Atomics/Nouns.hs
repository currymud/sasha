module Grammar.Parser.Rules.Atomics.Nouns (DirectionalStimulusNounRule (DirectionalStimulusNounRule
                                                                         , _directionalStimulusRule),
                                           EdibleNounRule (EdibleNounRule
                                                          , _edibleNounRule)) where
import           Data.Kind                  (Type)
import           Data.Text                  (Text)
import           Model.Parser.Atomics.Nouns (DirectionalStimulus, Edible)
import           Model.Parser.Lexer         (Lexeme)
import           Text.Earley                (Prod)

type DirectionalStimulusNounRule :: (Type -> Type -> Type -> Type) -> Type
newtype DirectionalStimulusNounRule r = DirectionalStimulusNounRule
  { _directionalStimulusRule :: Prod r Text Lexeme DirectionalStimulus}

type EdibleNounRule :: (Type -> Type -> Type -> Type) -> Type
newtype EdibleNounRule r = EdibleNounRule
  { _edibleNounRule :: Prod r Text Lexeme Edible }
