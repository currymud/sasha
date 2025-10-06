{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module ConstraintRefinement.Actions.Objects.Pill.Get (getPillDeniedF,alreadyHavePillF) where
import           Control.Monad.Identity     (Identity)
import           Data.Text                  (Text)
import           GameState                  (getObjectM)

import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (AcquisitionActionF (NotGettableF),
                                             ActionEffectKey (AcquisitionalActionKey),
                                             ActionManagementFunctions,
                                             ContainerAccessActionF,
                                             GameComputation,
                                             Object (_objectActionManagement))
import           Model.GID                  (GID)

alreadyHavePillF :: GID Object -> AcquisitionActionF
alreadyHavePillF oid = NotGettableF havePill
  where
    havePill :: (ActionManagementFunctions -> Maybe (GID AcquisitionActionF))
                  -> GameComputation Identity ActionEffectKey
    havePill lookupActionF = do
      actionManagement <- _objectActionManagement <$> getObjectM oid
      case lookupActionF actionManagement of
        Nothing -> error ("Programmer Error: No container access action found for object " ++ show oid)
        Just actionGID ->
          let actionKey = AcquisitionalActionKey actionGID
          in pure actionKey

getPillDeniedF :: GID Object -> AcquisitionActionF
getPillDeniedF oid = NotGettableF denied
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
