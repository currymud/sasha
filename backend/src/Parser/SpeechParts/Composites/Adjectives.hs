module Parser.SpeechParts.Composites.Adjectives where

import           Control.Applicative                   (Alternative ((<|>)))
import           Data.Kind                             (Type)
import           Data.Text                             (Text)
import           Lexer                                 (Lexeme)
import           Parser.SpeechParts.Atomics.Adjectives (Adjective)
import           Text.Earley.Grammar                   (Grammar, Prod, rule)

type AdjPhrase :: Type
data AdjPhrase
  = SimpleAdjPhrase Adjective -- RED
  | AdjPhrase Adjective Adjective -- RED SHINY
  deriving stock (Show, Eq, Ord)

type AdjPhraseRules :: (Type -> Type -> Type -> Type) -> Type
data AdjPhraseRules r = AdjPhraseRules
  { _adjRule          :: Prod r Text Lexeme Adjective
  , _secondaryAdjRule :: Prod r Text Lexeme Adjective
  }

adjPhraseRule :: AdjPhraseRules r
                  -> Grammar r (Prod r Text Lexeme AdjPhrase)
adjPhraseRule (AdjPhraseRules {..}) =
  rule $ SimpleAdjPhrase <$> _adjRule
           <|> SimpleAdjPhrase <$>  _adjRule
           <|> AdjPhrase <$> _adjRule <*> _secondaryAdjRule
