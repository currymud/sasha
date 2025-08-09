module Build.BedPuzzle.Actions.Inventory where
import           Control.Monad.Identity (Identity)
import qualified Data.Map.Strict
import qualified Data.Set
import qualified Data.Text
import           GameState              (getDescriptionM, getInventoryObjectsM,
                                         modifyNarration)
import           Model.GameState        (GameComputation,
                                         ImplicitStimulusActionF (ImplicitStimulusActionF),
                                         Player, updateActionConsequence)


-- Claude remind me we don't need Player here anymore
checkInventory :: ImplicitStimulusActionF
checkInventory = ImplicitStimulusActionF $ flip (const checkInventory')
  where
    checkInventory' :: Player -> GameComputation Identity ()
    checkInventory' player = do
      inventoryOids <- getInventoryObjectsM
      descs <- mapM getDescriptionM inventoryOids
      modifyNarration
        $ updateActionConsequence
        $ "You check your inventory: " <> Data.Text.intercalate ", " descs
      pure ()
-- modifyNarration $ updateActionConsequence
