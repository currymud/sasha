module ConstraintRefinement.Actions.Player.Inventory (defaultInventoryLookF, notEvenInventoryF) where

import           Control.Monad.Identity     (Identity)
import           Data.Kind                  (Type)
import           Data.Set                   (Set, toList)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           GameState.ActionManagement (processEffects, processInventoryNarration)
import           Model.Core                 (ActionEffectKey,
                                             GameComputation,
                                             ImplicitStimulusActionF (ImplicitStimulusActionF),
                                             InventoryFlavorText (InventoryFlavorText, _emptyFlavorText, _inventoryFlavorText))
import           Model.GID                  (GID)


defaultFlavorText :: InventoryFlavorText
defaultFlavorText = InventoryFlavorText
  { _emptyFlavorText     ="You've got nothing but a terrible headache and a slight pang of regret."
  , _inventoryFlavorText = "You look through your inventory. You have a feeling all these things are very important somehow."
  }

notEvenInventoryF :: ImplicitStimulusActionF
notEvenInventoryF = ImplicitStimulusActionF notEvenInventory'
  where
    notEvenInventory' :: Set ActionEffectKey -> GameComputation Identity ()
    notEvenInventory' effectKeys = do
      processEffects (Data.Set.toList effectKeys)
      -- Use the pre-defined empty inventory narration
      processInventoryNarration (InventoryFlavorText "You've got nothing but a terrible headache and a slight pang of regret." "")

defaultInventoryLookF :: ImplicitStimulusActionF
defaultInventoryLookF = inventoryLookF defaultFlavorText

inventoryLookF :: InventoryFlavorText -> ImplicitStimulusActionF
inventoryLookF (InventoryFlavorText {..}) = ImplicitStimulusActionF inventoryLook'
  where
    inventoryLook' :: Set ActionEffectKey -> GameComputation Identity ()
    inventoryLook' effectKeys  = do
      processEffects (Data.Set.toList effectKeys)
      processInventoryNarration (InventoryFlavorText _emptyFlavorText _inventoryFlavorText)
