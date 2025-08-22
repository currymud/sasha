module Build.BedPuzzle.Actions.Inventory (checkInventoryF,notEvenInventoryF) where
import           Control.Monad.Identity (Identity)
import qualified Data.Map.Strict
import qualified Data.Set
import qualified Data.Text
import           GameState              (getDescriptionM, getInventoryObjectsM,
                                         modifyNarration)
import           Model.GameState        (GameComputation,
                                         ImplicitStimulusActionF (ImplicitStimulusActionF),
                                         Player, updateActionConsequence)

notEvenInventoryF :: ImplicitStimulusActionF
notEvenInventoryF = ImplicitStimulusActionF (const (const notEvenInventory'))
  where
    notEvenInventory' :: GameComputation Identity ()
    notEvenInventory'  = do
      modifyNarration $ updateActionConsequence "You've got nothing but a terrible headache and a slight pang of regret."


-- checkInventory is okay for now, but we'll have to figure out if we want info from player or gameState
checkInventoryF :: ImplicitStimulusActionF
checkInventoryF = ImplicitStimulusActionF $ flip (const checkInventory')
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
