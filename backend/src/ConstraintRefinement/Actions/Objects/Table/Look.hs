module ConstraintRefinement.Actions.Objects.Table.Look where
import           Control.Monad.Identity     (Identity)
import           Data.Text                  (Text)
import           GameState                  (modifyNarration,
                                             updateActionConsequence)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ActionEffectKey,
                                             DirectionalStimulusActionF (CannotSeeF),
                                             GameComputation)
whatTable :: DirectionalStimulusActionF
whatTable = CannotSeeF whatTable'
  where
    whatTable' :: ActionEffectKey -> GameComputation Identity ()
    whatTable' actionEffectKey = do
      processEffectsFromRegistry actionEffectKey
      modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "It would be alot easier to see the table if you would open your eyes. Literally."

seeTable :: DirectionalStimulusActionF
seeTable = CannotSeeF seeTable'
  where
    seeTable' :: ActionEffectKey -> GameComputation Identity ()
    seeTable' actiolEffectKey = do
      processEffectsFromRegistry actiolEffectKey
      modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You see a table. It is a table. It is not a very interesting table, but it is a table nonetheless."
