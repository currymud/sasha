module ConstraintRefinement.Actions.Objects.Pocket.Open (notEvenOpenF, pocketOutOfReachF) where
import           Control.Monad.Identity     (Identity)
import           Data.Text                  (Text)
import           GameState                  (modifyNarration,
                                             updateActionConsequence)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ActionEffectKey,
                                             ContainerAccessActionF (CannotAccessF),
                                             GameComputation)

notEvenOpenF :: ContainerAccessActionF
notEvenOpenF = CannotAccessF notEvenOpen
  where
    notEvenOpen :: ActionEffectKey -> GameComputation Identity ()
    notEvenOpen actionEffectKey = do
      processEffectsFromRegistry actionEffectKey
      modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "This is rather difficult as your eyes are closed"

pocketOutOfReachF :: ContainerAccessActionF
pocketOutOfReachF = CannotAccessF outOfReach
  where
    outOfReach :: ActionEffectKey -> GameComputation Identity ()
    outOfReach actionEffectKey = do
      processEffectsFromRegistry actionEffectKey
      modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You'll need to get the robe first"
