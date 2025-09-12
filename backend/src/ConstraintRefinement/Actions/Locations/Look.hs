module ConstraintRefinement.Actions.Locations.Look where
import           Control.Monad.Identity     (Identity)
import qualified Data.List
import           Data.Set                   (Set)
import qualified Data.Set
import           GameState.ActionManagement (processEffects)
import           Model.Core                 (ActionEffectKey, GameComputation,
                                             ImplicitStimulusActionF (CannotSeeImplicitF, ImplicitStimulusActionF))

pitchBlackF :: ImplicitStimulusActionF
pitchBlackF = CannotSeeImplicitF pitchBlack
  where
    pitchBlack :: ActionEffectKey -> GameComputation Identity ()
    pitchBlack effectKey = do
      -- Just process the incoming effects (narration comes from effect system)
      processEffects (Data.List.singleton effectKey)

lookF :: ImplicitStimulusActionF
lookF = ImplicitStimulusActionF look
  where
    look :: ActionEffectKey -> GameComputation Identity ()
    look effectKey = do
      -- Process the effects (perception comes from SomaticNarrationEffect in effect system)
      processEffects (Data.List.singleton effectKey)
