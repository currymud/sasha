module Grammar.Parser.Rules.Atomics.Nouns ( ConsumablesRule (ConsumablesRule, _consumableRule)
                                           , DirectionalStimulusNounRule (DirectionalStimulusNounRule
                                                                         , _directionalStimulusRule)
                                           ,  SomaticStimulusRule (SomaticStimulusRule
                                                                   ,_somaticStimulusRule)) where
import           Data.Kind                  (Type)
import           Data.Text                  (Text)
import           Model.Parser.Atomics.Nouns (Consumable, DirectionalStimulus,
                                             SomaticStimulus)
import           Model.Parser.Lexer         (Lexeme)
import           Text.Earley                (Prod)

type DirectionalStimulusNounRule :: (Type -> Type -> Type -> Type) -> Type
newtype DirectionalStimulusNounRule r = DirectionalStimulusNounRule
  { _directionalStimulusRule :: Prod r Text Lexeme DirectionalStimulus}

type ConsumablesRule :: (Type -> Type -> Type -> Type) -> Type
newtype ConsumablesRule r = ConsumablesRule
  { _consumableRule :: Prod r Text Lexeme Consumable }

type SomaticStimulusRule :: (Type -> Type -> Type -> Type) -> Type
newtype SomaticStimulusRule r = SomaticStimulusRule
  { _somaticStimulusRule :: Prod r Text Lexeme SomaticStimulus }
