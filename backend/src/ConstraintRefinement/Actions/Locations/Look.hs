module ConstraintRefinement.Actions.Locations.Look where
import           Control.Monad.Identity     (Identity)
import qualified Data.Set
import           Data.Set                   (Set)
import           GameState.ActionManagement (processEffects)
import           GameState.Perception       (youSeeM)
import           Model.Core                 (ActionEffectKey, GameComputation,
                                             ImplicitStimulusActionF (CannotSeeImplicitF, ImplicitStimulusActionF))

pitchBlackF :: ImplicitStimulusActionF
pitchBlackF = CannotSeeImplicitF pitchBlack
  where
    pitchBlack :: Set ActionEffectKey -> GameComputation Identity ()
    pitchBlack effectKeys = do
      -- Just process the incoming effects (narration comes from effect system)
      processEffects (Data.Set.toList effectKeys)

lookF :: ImplicitStimulusActionF
lookF = ImplicitStimulusActionF look
  where
    look :: Set ActionEffectKey -> GameComputation Identity ()
    look effectKeys = do
      -- Get perception effects
      lookEffects <- youSeeM
      -- Combine incoming effects with perception effects
      let allEffects = Data.Set.toList effectKeys <> Data.Set.toList lookEffects
      -- Process all effects together
      processEffects allEffects
