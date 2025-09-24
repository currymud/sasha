module ConstraintRefinement.Effects.Player.Inventory where

import Model.Core (Effect (NarrationEffect), NarrationComputation (InventoryNarration))

inventoryNarrationEffect :: Effect
inventoryNarrationEffect = NarrationEffect InventoryNarration