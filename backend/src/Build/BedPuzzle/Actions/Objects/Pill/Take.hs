module Build.BedPuzzle.Actions.Objects.Pill.Take (takePillF, takePillDeniedF) where

import           Control.Monad.Identity        (Identity)
import qualified Data.Set
import           Data.Text                     (Text)
import           GameState                     (modifyNarration,
                                                modifySpatialRelationshipsForObjectM)
import           Model.GameState               (ActionEffectMap,
                                                ConsumptionActionF (ConsumptionActionF),
                                                GameComputation, Object,
                                                SpatialRelationship (ContainedIn, SupportedBy),
                                                updateActionConsequence)
import           Model.GID                     (GID)
import           Model.Parser.Composites.Verbs (ConsumptionVerbPhrase)

takePillDeniedF :: ConsumptionActionF
takePillDeniedF = ConsumptionActionF (const (const (const denied)))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You can't take the pill right now."

takePillF :: ConsumptionActionF
takePillF = ConsumptionActionF takeIt
  where
    takeIt :: GID Object -> ActionEffectMap -> ConsumptionVerbPhrase -> GameComputation Identity ()
    takeIt objectGID _actionEffectMap _cvp = do
      -- Remove the pill from whatever is containing/supporting it
      modifySpatialRelationshipsForObjectM objectGID $ \relationships ->
        Data.Set.filter (not . isLocationRelationship) relationships

      -- Fire the consumption consequence
      modifyNarration $ updateActionConsequence "You take the pill and swallow it."

    isLocationRelationship :: SpatialRelationship -> Bool
    isLocationRelationship (ContainedIn _) = True
    isLocationRelationship (SupportedBy _) = True
    isLocationRelationship _               = False
