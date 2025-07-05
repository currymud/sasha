module Model.Mappings where

import           Data.Kind          (Type)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Map.Strict    (Map)

import           Model.GID          (GID)
import           Model.Label        (Label)

type role GIDToDataMap nominal representational
type GIDToDataMap :: Type -> Type -> Type
newtype GIDToDataMap a b
  = GIDToDataMap {_getGIDToDataMap :: Map (GID a) b}
      deriving stock Show

type role GIDList phantom
type GIDList :: Type -> Type
newtype GIDList a = GIDList { _getGIDList :: NonEmpty (GID a)}
 deriving stock Show

type role LabelToGIDListMapping nominal representational
type LabelToGIDListMapping :: Type -> Type -> Type
newtype LabelToGIDListMapping a b
  = LabelToGIDListMapping
    { _getLabelToGIDListMapping :: Map (Label a) (GIDList b)}
      deriving stock Show
