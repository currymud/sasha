module Build.BedPuzzle.Actions.Objects.Pill.Take (alreadyTookPillF, pillTooFarF, takePillF, takePillDeniedF) where

import           Control.Monad.Identity        (Identity)
import           Control.Monad.State           (modify')
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict
import           Data.Set                      (Set)
import qualified Data.Set
import           Data.Text                     (Text)
import           GameState                     (modifyNarration,
                                                modifyObjectActionManagementM,
                                                modifySpatialRelationshipsForObjectM)
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
takePillF = ConsumptionActionF takeIt
  where
    takeIt :: GID Object -> Set ActionEffectKey -> ActionEffectMap -> ConsumptionVerbPhrase -> GameComputation Identity ()
    takeIt pillOID actionEffectKeys (ActionEffectMap actionEffectMap) _cvp = do
      -- Remove the pill from whatever is containing/supporting it
      modifySpatialRelationshipsForObjectM pillOID $ \relationships ->
        Data.Set.filter (not . isLocationRelationship) relationships

      -- Fire the consumption consequence
      modifyNarration $ updateActionConsequence "You take the pill and swallow it."

      -- Process effects for all action effect keys
      mapM_ (processEffectKeyEntry actionEffectMap) (Data.Set.toList actionEffectKeys)

    processEffectKeyEntry :: (Map ActionEffectKey (Set Effect))
                          -> ActionEffectKey
                          -> GameComputation Identity ()
    processEffectKeyEntry effectMap actionEffectKey = do
      case Data.Map.Strict.lookup actionEffectKey effectMap of
        Nothing -> pure ()
        Just effects -> mapM_ (processEffect actionEffectKey) (Data.Set.toList effects)

    processEffect :: ActionEffectKey -> Effect -> GameComputation Identity ()
    processEffect (PlayerKey (PlayerKeyObject oid)) effect = pure ()
      -- Update the player's positive postural actions

    processEffect (PlayerKey (PlayerKeyObject oid)) effect  = pure ()

    processEffect (ObjectKey oid) (DirectionalStimulusEffect verb newActionGID) = pure ()

    processEffect _ _ = pure () -- Handle other effect types as needed

    isLocationRelationship :: SpatialRelationship -> Bool
    isLocationRelationship (ContainedIn _) = True
    isLocationRelationship (SupportedBy _) = True
    isLocationRelationship _               = False
