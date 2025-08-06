module Build.BedPuzzle.Actions.Inventory where
import           Control.Monad.Identity (Identity)
import qualified Data.Map.Strict
import qualified Data.Set
import qualified Data.Text
import           GameState              (getDescriptionM, modifyNarration)
import           Model.GameState        (GameComputation,
                                         ImplicitStimulusActionF (ImplicitStimulusActionF),
                                         Player (_inventory),
                                         updateActionConsequence)

checkInventory :: ImplicitStimulusActionF
checkInventory = ImplicitStimulusActionF $ flip (const checkInventory')
  where
    checkInventory' :: Player -> GameComputation Identity ()
    checkInventory' player = do
      descs <- mapM getDescriptionM inventory
      modifyNarration
        $ updateActionConsequence
        $ "You check your inventory: " <> Data.Text.intercalate ", " descs
      pure ()
      where
        inventory = mconcat $ Data.Set.toList
                      <$> Data.Map.Strict.elems player._inventory
-- modifyNarration $ updateActionConsequence
