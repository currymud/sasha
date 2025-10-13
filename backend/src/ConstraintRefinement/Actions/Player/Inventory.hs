module ConstraintRefinement.Actions.Player.Inventory (defaultInventoryLookF, notEvenInventoryF) where

import           Control.Monad.Identity     (Identity)
import           Data.Kind                  (Type)
import           Data.Text                  (Text)
import           GameState                  (getObjectM)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ActionEffectKey, GameComputation,
                                             ImplicitStimulusActionF (CannotImplicitStimulusActionF, PlayerImplicitStimulusActionF),
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
notEvenInventoryF = CannotImplicitStimulusActionF notEvenInventory'
  where
    notEvenInventory' :: ActionEffectKey -> GameComputation Identity ()
    notEvenInventory' actionEffectKey  = do
      -- All narration now handled via effects
      processEffectsFromRegistry actionEffectKey

defaultInventoryLookF :: ImplicitStimulusActionF
defaultInventoryLookF = inventoryLookF defaultFlavorText

inventoryLookF :: InventoryFlavorText -> ImplicitStimulusActionF
inventoryLookF (InventoryFlavorText {..}) = PlayerImplicitStimulusActionF inventoryLook'
  where
    inventoryLook' :: ActionEffectKey ->  GameComputation Identity ()
    inventoryLook' actionEffectKey  = do
      -- All narration now handled via effects
      processEffectsFromRegistry actionEffectKey

getObjectShortName :: GID Object -> GameComputation Identity Text
getObjectShortName oid = do
  obj <- getObjectM oid
  pure $ _shortName obj
