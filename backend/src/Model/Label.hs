module Model.Label (Label (..)) where

import           Data.Kind                (Type)
import           Data.Text                (Text, pack)
import           Grammar.Parser.Lexer     (Lexeme)
import           Relude.String.Conversion (ToText (toText))

type role Label phantom
type Label :: Type -> Type
newtype Label a = Label {unLabel :: Lexeme}
  deriving newtype (Show, Ord, Eq)

instance ToText (Label a) where
  toText :: Label a -> Text
  toText = pack . show  . unLabel
