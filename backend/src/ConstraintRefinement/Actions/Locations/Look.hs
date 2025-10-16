module ConstraintRefinement.Actions.Locations.Look where
import           Control.Monad.Identity     (Identity)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ActionEffectKey,
                                             AgentImplicitStimulusActionF (AgentCannotSeeF),
                                             GameComputation,
                                             ImplicitStimulusActionF (CannotImplicitStimulusActionF, PlayerImplicitStimulusActionF),
                                             LocationDirectionalStimulusActionF (LocationCanBeSeenF),
                                             LocationImplicitStimulusActionF (LocationCanBeSeenImplicitF, LocationCannotBeSeenImplicitF))
-- deprecated
pitchBlackF :: ImplicitStimulusActionF
pitchBlackF = CannotImplicitStimulusActionF pitchBlack'
  where
    pitchBlack' :: ActionEffectKey -> GameComputation Identity ()
    pitchBlack' actionEffectKey = do
      processEffectsFromRegistry actionEffectKey

locationNoSeeF :: LocationImplicitStimulusActionF
locationNoSeeF = LocationCannotBeSeenImplicitF processEffectsFromRegistry

lookF :: ImplicitStimulusActionF
lookF = PlayerImplicitStimulusActionF look
  where
    look :: ActionEffectKey -> GameComputation Identity ()
    look actionEffectKey = do
      processEffectsFromRegistry actionEffectKey

allowLookF :: LocationImplicitStimulusActionF
allowLookF = LocationCanBeSeenImplicitF processEffectsFromRegistry

locationAllowLookAtF :: LocationDirectionalStimulusActionF
locationAllowLookAtF = LocationCanBeSeenF processEffectsFromRegistry

