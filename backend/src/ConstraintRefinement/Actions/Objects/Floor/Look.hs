module ConstraintRefinement.Actions.Objects.Floor.Look
    (notEvenFloorF) where
import           Control.Monad.Identity     (Identity)
import           Data.Text                  (Text)
import           GameState                  (modifyNarration,
                                             updateActionConsequence)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ActionEffectKey,
                                             DirectionalStimulusActionF (CannotSeeF),
                                             GameComputation)

notEvenFloorF :: DirectionalStimulusActionF
notEvenFloorF = CannotSeeF notEvenFloor
  where
    notEvenFloor :: ActionEffectKey -> GameComputation Identity ()
    notEvenFloor actionEffectKey = do
      processEffectsFromRegistry actionEffectKey
      modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "One step at a time champ. You had a rough night. Open your eyes first."
