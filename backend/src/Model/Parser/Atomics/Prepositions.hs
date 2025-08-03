module Model.Parser.Atomics.Prepositions ( ContainmentMarker (ContainmentMarker)
                                          ,DirectionalStimulusMarker (DirectionalStimulusMarker)
                                         , SourceMarker (SourceMarker)
                                         , SurfaceMarker (SurfaceMarker)) where
import           Data.Hashable            (Hashable)
import           Data.Kind                (Type)
import           GHC.Generics             (Generic)
import           Grammar.Parser.Lexer     (HasLexeme (toLexeme))
import           Model.Parser.Lexer       (Lexeme)
import           Relude.String.Conversion (ToText)

type ContainmentMarker :: Type
newtype ContainmentMarker = ContainmentMarker { _fromContainmentMarker :: Lexeme }
  deriving stock (Show,Eq,Ord,Generic)
  deriving newtype (Hashable,ToText)

instance HasLexeme ContainmentMarker where
  toLexeme = _fromContainmentMarker

type DirectionalStimulusMarker :: Type
newtype DirectionalStimulusMarker
  = DirectionalStimulusMarker { _fromDirectionalStimulusMarker :: Lexeme }
  deriving stock (Show,Eq,Ord,Generic)
  deriving newtype (Hashable,ToText)

instance HasLexeme DirectionalStimulusMarker where
  toLexeme = _fromDirectionalStimulusMarker

type SourceMarker :: Type
newtype SourceMarker = SourceMarker { _fromSourceMarker :: Lexeme }
  deriving stock (Show,Eq,Ord,Generic)
  deriving newtype (Hashable,ToText)

instance HasLexeme SourceMarker where
  toLexeme = _fromSourceMarker

type SurfaceMarker :: Type
newtype SurfaceMarker = SurfaceMarker { _fromSurfaceMarker :: Lexeme }
  deriving stock (Show,Eq,Ord,Generic)
  deriving newtype (Hashable,ToText)

instance HasLexeme SurfaceMarker where
  toLexeme = _fromSurfaceMarker

