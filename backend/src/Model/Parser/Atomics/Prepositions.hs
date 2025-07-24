module Model.Parser.Atomics.Prepositions where
import           Data.Hashable            (Hashable)
import           Data.Kind                (Type)
import           GHC.Generics             (Generic)
import           Grammar.Parser.Lexer     (HasLexeme (toLexeme))
import           Model.Parser.Lexer       (Lexeme)
import           Relude.String.Conversion (ToText)

type DirectionalStimulusMarker :: Type
newtype DirectionalStimulusMarker
  = DirectionalStimulusMarker { _fromDirectionalStimulusMarker :: Lexeme }
  deriving stock (Show,Eq,Ord,Generic)
  deriving newtype (Hashable,ToText)

instance HasLexeme DirectionalStimulusMarker where
  toLexeme = _fromDirectionalStimulusMarker

