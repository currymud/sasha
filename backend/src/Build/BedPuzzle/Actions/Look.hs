module Build.BedPuzzle.Actions.Look (lookAtF) where
import           Control.Monad.Identity (Identity)
import           GameState              (getObjectM, modifyNarration)
import           Model.GameState        (DirectionalStimulusActionF (DirectionalStimulusActionF),
                                         GameComputation, Object (_description),
                                         updateActionConsequence)
import           Model.GID              (GID)

lookAtF :: GID Object -> DirectionalStimulusActionF
lookAtF oid = DirectionalStimulusActionF (const (const lookAt'))
  where
    lookAt' :: GameComputation Identity ()
    lookAt' = do
      description <- _description <$> getObjectM oid
      modifyNarration $ updateActionConsequence description
