module ConstraintRefinement.Actions.Locations.Look where
import           Control.Monad.Identity (Identity)
import           Data.Set               (Set)
import           GameState.Perception   (youSeeM)
import           Model.Core             (EffectKey, GameComputation,
                                         ImplicitStimulusActionF (CannotSeeImplicitF, ImplicitStimulusActionF))

pitchBlackF :: ImplicitStimulusActionF
pitchBlackF = CannotSeeImplicitF pitchBlack
  where
    pitchBlack :: Set EffectKey -> GameComputation Identity ()
    pitchBlack _ = do
      -- process all effects here
      pure ()

lookF :: ImplicitStimulusActionF
lookF = ImplicitStimulusActionF look
  where
    look :: Set EffectKey -> GameComputation Identity ()
    look effects = do
      -- process all effects here
      lookEffects <- youSeeM
      -- combine lookeffects with effects and process them
      pure () -- placeholder remove when completed
