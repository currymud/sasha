module ConstraintRefinement.Actions.Player.Inventory (defaultInventoryLookF, notEvenInventoryF) where

import           Control.Monad.Identity (Identity)
import           Data.Kind              (Type)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           GameState              (getInventoryObjectsM, getObjectM,
                                         modifyNarration,
                                         updateActionConsequence)
import           Model.Core             (GameComputation,
                                         ImplicitStimulusActionF (ImplicitStimulusActionF),
                                         Object (_shortName))
import           Model.GID              (GID)

type InventoryFlavorText :: Type
data InventoryFlavorText = InventoryFlavorText
  { _emptyFlavorText     :: Text
  , _inventoryFlavorText :: Text
  }

defaultFlavorText :: InventoryFlavorText
defaultFlavorText = InventoryFlavorText
  { _emptyFlavorText     ="You've got nothing but a terrible headache and a slight pang of regret."
  , _inventoryFlavorText = "You look through your inventory. You have a feeling all these things are very important somehow."
  }

notEvenInventoryF :: ImplicitStimulusActionF
notEvenInventoryF = ImplicitStimulusActionF (const (const notEvenInventory'))
  where
    notEvenInventory' :: GameComputation Identity ()
    notEvenInventory'  = do
      modifyNarration $ updateActionConsequence "You've got nothing but a terrible headache and a slight pang of regret."

defaultInventoryLookF :: ImplicitStimulusActionF
defaultInventoryLookF = inventoryLookF defaultFlavorText

inventoryLookF :: InventoryFlavorText -> ImplicitStimulusActionF
inventoryLookF (InventoryFlavorText {..}) = ImplicitStimulusActionF inventoryLook'
  where
    inventoryLook' :: a -> b -> GameComputation Identity ()
    inventoryLook' _player _location = do
      inventoryObjects <- getInventoryObjectsM
      case inventoryObjects of
        [] -> do
          -- Empty inventory case
          modifyNarration $ updateActionConsequence _emptyFlavorText
        objects -> do
          -- Non-empty inventory case
          objectNames <- mapM getObjectShortName objects
          let itemsList = Text.intercalate ", " objectNames
              fullMessage = _inventoryFlavorText <> " You are carrying: " <> itemsList <> "."
          modifyNarration $ updateActionConsequence fullMessage
-- | Get the short name of an object
getObjectShortName :: GID Object -> GameComputation Identity Text
getObjectShortName oid = do
  obj <- getObjectM oid
  pure $ _shortName obj
