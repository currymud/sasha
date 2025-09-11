module ConstraintRefinement.Actions.Player.Inventory (defaultInventoryLookF, notEvenInventoryF) where

import           Control.Monad.Identity     (Identity)
import           Data.Kind                  (Type)
import           Data.Set                   (Set, toList)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           GameState                  (getInventoryObjectsM, getObjectM)
import           GameState.ActionManagement (processEffects)
import           Model.Core                 (EffectKey (NarrationKey),
                                             GameComputation,
                                             ImplicitStimulusActionF (ImplicitStimulusActionF),
                                             NarrationOperation (ActionConsequenceNarration),
                                             Object (_shortName))
import           Model.GID                  (GID)

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
notEvenInventoryF = ImplicitStimulusActionF notEvenInventory'
  where
    notEvenInventory' :: Set EffectKey -> GameComputation Identity ()
    notEvenInventory' effectKeys = do
      let narrationEffect = NarrationKey (ActionConsequenceNarration "You've got nothing but a terrible headache and a slight pang of regret.")
          allEffects = Data.Set.toList effectKeys <> [narrationEffect]
      processEffects allEffects

defaultInventoryLookF :: ImplicitStimulusActionF
defaultInventoryLookF = inventoryLookF defaultFlavorText

inventoryLookF :: InventoryFlavorText -> ImplicitStimulusActionF
inventoryLookF (InventoryFlavorText {..}) = ImplicitStimulusActionF inventoryLook'
  where
    inventoryLook' :: Set EffectKey -> GameComputation Identity ()
    inventoryLook' effectKeys  = do
      inventoryObjects <- getInventoryObjectsM
      narrationEffect <- case inventoryObjects of
        [] -> pure $ NarrationKey (ActionConsequenceNarration _emptyFlavorText)
        objects -> do
          objectNames <- mapM getObjectShortName objects
          let itemsList = Text.intercalate ", " objectNames
              fullMessage = _inventoryFlavorText <> " You are carrying: " <> itemsList <> "."
          pure $ NarrationKey (ActionConsequenceNarration fullMessage)

      let allEffects = Data.Set.toList effectKeys <> [narrationEffect]
      processEffects allEffects
-- | Get the short name of an object
getObjectShortName :: GID Object -> GameComputation Identity Text
getObjectShortName oid = do
  obj <- getObjectM oid
  pure $ _shortName obj
