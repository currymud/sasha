module ConstraintRefinement.Actions.Player.Inventory () where

import           Control.Monad.Identity     (Identity)
import           Data.Text                  (Text)
import           GameState                  (getObjectM)
import           Model.Core                 (GameComputation,
                                             Object (_shortName))
import           Model.GID                  (GID)

getObjectShortName :: GID Object -> GameComputation Identity Text
getObjectShortName oid = do
  obj <- getObjectM oid
  pure $ _shortName obj
