{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Model.Parser.Composites.Nouns where
import           Data.Kind                       (Type)
import           Data.Text                       (Text, unwords)
import           GHC.Generics                    (Generic)
import           Model.Parser.Atomics.Adjectives (Adjective)
import           Model.Parser.Atomics.Misc       (Determiner)
import           Model.Parser.Atomics.Nouns      (DirectionalStimulus)
import           Model.Parser.Lexer              (Lexeme)
import           Prelude                         hiding (unwords)
import           Relude.String.Conversion        (ToText (toText))
import           Text.Earley                     (Prod)

type DirectionalStimulusNounPhrase :: Type
newtype DirectionalStimulusNounPhrase = DirectionalStimulusNounPhrase (NounPhrase DirectionalStimulus)
  deriving stock (Show, Eq, Ord,Generic)
  deriving newtype (ToText)

type NounPhrase :: Type -> Type
data NounPhrase a
  = SimpleNounPhrase a
  | NounPhrase Determiner a
  | DescriptiveNounPhrase Adjective a
  | DescriptiveNounPhraseDet Determiner Adjective a
  deriving stock (Show, Eq, Ord,Generic)

instance ToText a => ToText (NounPhrase a) where
  toText (SimpleNounPhrase a) = toText a
  toText (NounPhrase det a) = unwords [toText det, toText a]
  toText (DescriptiveNounPhrase adj a) =
    unwords [toText adj, toText a]
  toText (DescriptiveNounPhraseDet det adj a) =
    unwords [toText det, toText adj, toText a]

type NounPhraseRules :: Type -> (Type -> Type -> Type -> Type) -> Type
data NounPhraseRules a r = NounPhraseRules
  { _determinerRule :: Prod r Text Lexeme Determiner
  , _adjRule        :: Prod r Text Lexeme Adjective
  , _nounRule       :: Prod r Text Lexeme a
  }

