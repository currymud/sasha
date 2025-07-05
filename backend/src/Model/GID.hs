module Model.GID where

import           Data.Kind                (Type)
import           Data.Text                (Text, pack)
import           Relude.String.Conversion (ToText (toText))

type role GID phantom
type GID :: Type -> Type
newtype GID a = GID {unGID :: Int}
  deriving newtype (Show, Ord, Eq)

instance ToText (GID a) where
  toText :: GID a -> Text
  toText = pack . show
