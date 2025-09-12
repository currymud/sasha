module ConstraintRefinement.Actions.Player.Inventory (defaultInventoryLookF, notEvenInventoryF) where

import           Control.Monad.Identity     (Identity)
import qualified Data.List
import           Data.Set                   (Set, toList)
import           GameState.ActionManagement (processEffects,
                                             processInventoryNarration)
import           Model.Core                 (ActionEffectKey, GameComputation,
                                             ImplicitStimulusActionF (ImplicitStimulusActionF),
                                             InventoryFlavorText (InventoryFlavorText, _emptyFlavorText, _inventoryFlavorText))

defaultFlavorText :: InventoryFlavorText
defaultFlavorText = InventoryFlavorText
  { _emptyFlavorText     ="You've got nothing but a terrible headache and a slight pang of regret."
  , _inventoryFlavorText = "You look through your inventory. You have a feeling all these things are very important somehow."
  }

notEvenInventoryF :: ImplicitStimulusActionF
notEvenInventoryF = ImplicitStimulusActionF notEvenInventory'
  where
    notEvenInventory' :: ActionEffectKey -> GameComputation Identity ()
    notEvenInventory' effectKey = do
      processEffects (Data.List.singleton effectKey)
      -- Use the pre-defined empty inventory narration
      processInventoryNarration (InventoryFlavorText "You've got nothing but a terrible headache and a slight pang of regret." "")

defaultInventoryLookF :: ImplicitStimulusActionF
defaultInventoryLookF = inventoryLookF defaultFlavorText

inventoryLookF :: InventoryFlavorText -> ImplicitStimulusActionF
inventoryLookF (InventoryFlavorText {..}) = ImplicitStimulusActionF inventoryLook'
  where
    inventoryLook' :: ActionEffectKey -> GameComputation Identity ()
    inventoryLook' effectKey  = do
      processEffects (Data.List.singleton effectKey)
      processInventoryNarration (InventoryFlavorText _emptyFlavorText _inventoryFlavorText)
