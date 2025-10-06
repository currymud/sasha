{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module ConstraintRefinement.Actions.Objects.Floor.Get (getFloorDeniedF) where
import           Control.Monad.Identity (Identity)
import           GameState              (getObjectM)
import           Model.Core             (AcquisitionActionF (NotGettableF),
                                         ActionEffectKey (AcquisitionalActionKey),
                                         ActionManagementFunctions,
                                         GameComputation,
                                         Object (_objectActionManagement))
import           Model.GID              (GID)

getFloorDeniedF :: GID Object -> AcquisitionActionF
getFloorDeniedF oid = NotGettableF denied
  where
    denied :: (ActionManagementFunctions -> Maybe (GID AcquisitionActionF))
                -> GameComputation Identity ActionEffectKey
    denied lookupActionF = do
      actionManagement <- _objectActionManagement <$> getObjectM oid
      case lookupActionF actionManagement of
        Nothing -> error ("Programmer Error: No container access action found for object " ++ show oid)
        Just actionGID ->
          let actionKey = AcquisitionalActionKey actionGID
          in pure actionKey
--    msg :: Text
--    msg = "You try to pick up the floor. It doesn't budge. Physics wins again."
