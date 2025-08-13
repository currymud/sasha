module Build.BedPuzzle.Actions.Objects.Pill.Take (alreadyTookPillF, pillTooFarF, takePillF, takePillDeniedF) where

import           Control.Monad.Identity        (Identity)
import           Control.Monad.State           (modify')
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict
import           Data.Set                      (Set)
import qualified Data.Set
import           Data.Text                     (Text)
import           GameState                     (modifyNarration,
                                                modifySpatialRelationshipsForObjectM)
import           GameState.ActionManagement    (processAllEffects)
import           Model.GameState               (ActionEffectKey (ObjectKey, PlayerKey),
                                                ActionEffectMap (ActionEffectMap),
                                                ActionManagement (DSAManagementKey, NPManagementKey, PPManagementKey),
                                                ActionManagementFunctions (ActionManagementFunctions),
                                                ConsumptionActionF (ConsumptionActionF),
                                                Effect (DirectionalStimulusEffect, NegativePosturalEffect, PositivePosturalEffect),
                                                GameComputation,
                                                GameState (_player), Object,
                                                Player (_playerActions),
                                                PlayerKey (PlayerKeyObject),
                                                SpatialRelationship (ContainedIn, SupportedBy),
                                                updateActionConsequence)
import           Model.GID                     (GID)
import           Model.Parser.Composites.Verbs (ConsumptionVerbPhrase)

takePillDeniedF :: ConsumptionActionF
takePillDeniedF = ConsumptionActionF (const (const (const (const denied))))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You can't take the pill right now."


alreadyTookPillF :: ConsumptionActionF
alreadyTookPillF = ConsumptionActionF (const (const (const (const denied))))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You already took the pill."

pillTooFarF :: ConsumptionActionF
pillTooFarF = ConsumptionActionF (const (const (const (const denied))))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You grab at it but it's hard to get to. try grabbing the robe first."

takePillF :: ConsumptionActionF
takePillF = ConsumptionActionF (const (const (const (const (pure ())))))
