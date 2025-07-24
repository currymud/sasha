module Model.Parser.Atomics.Nouns where
import           Data.Hashable            (Hashable)
import           Data.Kind                (Type)
import           Data.Text                (Text)
import           Grammar.Parser.Lexer     (HasLexeme (toLexeme))
import           Model.Parser.Lexer       (Lexeme)
import           Relude.String.Conversion (ToText)
import           Text.Earley              (Prod)

type DirectionalStimulus :: Type
newtype DirectionalStimulus = DirectionalStimulus { _fromDirectionalStimulus :: Lexeme }
  deriving newtype (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme DirectionalStimulus where
  toLexeme = _fromDirectionalStimulus

type DirectionalStimulusRule :: (Type -> Type -> Type -> Type) -> Type
newtype DirectionalStimulusRule r = DirectionalStimulusRule
  { _directionalStimulusRule :: Prod r Text Lexeme DirectionalStimulus}

type Objective :: Type
newtype Objective = Objective { _fromObjective :: Lexeme }
  deriving newtype (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme Objective where
  toLexeme = _fromObjective
-- import           Grammar.Parser.Lexer       (HasLexeme (toLexeme),
--                                             Lexeme (BED, CHAIR, LAMP, PILL, POCKET, TABLE, MAIL, SATCHEL, ROBE, TOWEL))
type Supportive :: Type
newtype Supportive =
  Supportive { _fromSupportive :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme Supportive where
  toLexeme = _fromSupportive

type Container :: Type
newtype Container =
  Container { _fromContainer :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme Container where
  toLexeme = _fromContainer

type ToggleNoun :: Type
newtype ToggleNoun = ToggleNoun { _fromToggleNoun :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme ToggleNoun where
  toLexeme = _fromToggleNoun

type ModToggleNoun :: Type
newtype ModToggleNoun = ModToggleNoun { _fromModToggleNoun :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme ModToggleNoun where
  toLexeme = _fromModToggleNoun

type SimpleAccessNoun :: Type
newtype SimpleAccessNoun = SimpleAccessNoun { _fromSimpleAccessNoun :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme SimpleAccessNoun where
  toLexeme = _fromSimpleAccessNoun

