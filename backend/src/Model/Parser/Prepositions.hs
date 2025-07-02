module Model.Parser.Prepositions where
import           Data.Kind                         (Type)
import           Data.Text                         (Text)
import           Lexer                             (Lexeme)
import           Model.Parser.Atomics.Prepositions (LocationInterrogativeMarker,
                                                    TargetedStimulusMarker)
import           Text.Earley                       (Grammar)
import           Text.Earley.Grammar               (Prod)

type PrepParsers :: (Type -> Type -> Type -> Type) -> Type
data PrepParsers r = PrepParsers
  { _locationInterrogativeMarker' :: Grammar r (Prod r Text Lexeme LocationInterrogativeMarker)
  , _targetedStimulusMarker'        :: Grammar r (Prod r Text Lexeme TargetedStimulusMarker)
  }
