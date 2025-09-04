module Model.Parser.Atomics.Nouns where
import           Data.Hashable            (Hashable)
import           Data.Kind                (Type)
import           Data.Text                (Text)
import           Grammar.Parser.Lexer     (HasLexeme (toLexeme))
import           Model.Parser.Lexer       (Lexeme)
import           Relude.String.Conversion (ToText)
import           Text.Earley              (Prod)

type Consumable :: Type
newtype Consumable = Consumable { _fromConsumable :: Lexeme }
  deriving newtype (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme Consumable where
  toLexeme = _fromConsumable

type Container :: Type
newtype Container =
  Container { _fromContainer :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme Container where
  toLexeme = _fromContainer

type DirectionalStimulus :: Type
newtype DirectionalStimulus = DirectionalStimulus { _fromDirectionalStimulus :: Lexeme }
  deriving newtype (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme DirectionalStimulus where
  toLexeme = _fromDirectionalStimulus

type DirectionalStimulusRule :: (Type -> Type -> Type -> Type) -> Type
newtype DirectionalStimulusRule r = DirectionalStimulusRule
  { _directionalStimulusRule :: Prod r Text Lexeme DirectionalStimulus}

type ModToggleNoun :: Type
newtype ModToggleNoun = ModToggleNoun { _fromModToggleNoun :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme ModToggleNoun where
  toLexeme = _fromModToggleNoun

type Objective :: Type
newtype Objective = Objective { _fromObjective :: Lexeme }
  deriving newtype (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme Objective where
  toLexeme = _fromObjective

type PosturalVerb :: Type
newtype PosturalVerb = PosturalVerb { _fromPosturalVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme PosturalVerb where
  toLexeme = _fromPosturalVerb

type Potable :: Type
newtype Potable = Potable { _fromPotable :: Lexeme }
  deriving newtype (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme Potable where
  toLexeme = _fromPotable

type SomaticStimulus :: Type
newtype SomaticStimulus = SomaticStimulus { _fromSomaticStimulus :: Lexeme }
  deriving newtype (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme SomaticStimulus where
  toLexeme = _fromSomaticStimulus

type Supportive :: Type
newtype Supportive =
  Supportive { _fromSupportive :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme Supportive where
  toLexeme = _fromSupportive

type Surface :: Type
newtype Surface = Surface { _fromSurface :: Lexeme }
  deriving newtype (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme Surface where
  toLexeme = _fromSurface

type InstrumentalAccessNoun :: Type
newtype InstrumentalAccessNoun = InstrumentalAccessNoun { _fromInstrumentalAccessNoun :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme InstrumentalAccessNoun where
  toLexeme = _fromInstrumentalAccessNoun

type ToggleNoun :: Type
newtype ToggleNoun = ToggleNoun { _fromToggleNoun :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme ToggleNoun where
  toLexeme = _fromToggleNoun

type SimpleAccessNoun :: Type
newtype SimpleAccessNoun = SimpleAccessNoun { _fromSimpleAccessNoun :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme SimpleAccessNoun where
  toLexeme = _fromSimpleAccessNoun

