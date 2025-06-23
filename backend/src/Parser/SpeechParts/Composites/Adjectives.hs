module Parser.SpeechParts.Composites.Adjectives where

import           Control.Applicative                   (Alternative ((<|>)))
import           Data.Kind                             (Type)
import           Data.Text                             (Text, unwords)
import           GHC.Generics                          (Generic)
import           Lexer                                 (Lexeme)
import           Parser.SpeechParts.Atomics.Adjectives (Adjective)
import           Prelude                               hiding (unwords)
import           Relude.String.Conversion              (ToText (toText))
import           Text.Earley.Grammar                   (Grammar, Prod, rule)

#ifdef TESTING
import           Test.QuickCheck                       (Arbitrary (arbitrary))
import           Test.QuickCheck.Arbitrary.Generic     (GenericArbitrary (..))
import           Test.QuickCheck.Instances.Text        ()
#endif

type AdjPhrase :: Type
data AdjPhrase
  = SimpleAdjPhrase Adjective -- RED
  | AdjPhrase Adjective Adjective -- RED SHINY
  deriving stock (Show, Eq, Generic,Ord)

instance ToText AdjPhrase where
  toText (SimpleAdjPhrase adj) = toText adj
  toText (AdjPhrase adj1 adj2) = unwords [toText adj1, toText adj2]

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

#ifdef TESTING
deriving via GenericArbitrary AdjPhrase instance Arbitrary AdjPhrase
#endif
