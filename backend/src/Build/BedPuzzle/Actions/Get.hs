{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Get (get,getDenied) where
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.Identity    (Identity)
import qualified Data.Map.Strict
import           Data.Maybe                (catMaybes)
import           Data.Set                  (Set, fromList, toList)
import           Data.Text                 (Text)
import           GameState                 (getLocationM, modifyLocationM,
                                            modifyNarration,
                                            modifyObjectActionManagementM,
                                            updatePerceptionMapM, youSeeM)
import           Model.GameState           (AcquisitionActionF (AcquisitionActionF),
                                            ActionEffectKey (LocationKey, ObjectKey),
                                            ActionEffectMap (ActionEffectMap),
                                            ActionManagement (_directionalStimulusActionManagement, _implicitStimulusActionManagement, _somaticStimulusActionManagement),
                                            Effect (AcquisitionEffect, DirectionalStimulusEffect, ImplicitStimulusEffect, SomaticAccessEffect),
                                            GameComputation,
                                            Location (_locationActionManagement),
                                            Object, updateActionConsequence)
import           Model.GID                 (GID)


getDenied :: AcquisitionActionF
getDenied = AcquisitionActionF (const (const denied))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You try but feel dizzy and have to lay back down"

get :: AcquisitionActionF
get = AcquisitionActionF opened
  where
    opened :: Set ActionEffectKey ->  ActionEffectMap -> GameComputation Identity ()
    opened actionEffectKeys (ActionEffectMap actionEffectMap) = do
      mapM process (Data.Set.toList actionEffectKeys)
      youSeeM
      modifyNarration (updateActionConsequence msg)
      where
        process :: ActionEffectKey -> GameComputation Identity ()
        process actionEffectKey@(LocationKey lid) = do
          case Data.Map.Strict.lookup actionEffectKey actionEffectMap of
            Nothing -> throwError "No effect for actionEffectKey found in actionEffectMap"
            Just effects -> mapM_ handleEffect effects
            where
              handleEffect :: Effect -> GameComputation Identity ()
              handleEffect (ImplicitStimulusEffect implicitStimulusVerb changeTo) = do
                modifyLocationM lid $ \loc ->
                  let actionMgmt = _locationActionManagement loc
                      implicitMap = _implicitStimulusActionManagement actionMgmt
                      updatedImplicitMap = Data.Map.Strict.insert implicitStimulusVerb changeTo implicitMap
                      updatedActionMgmt = actionMgmt { _implicitStimulusActionManagement = updatedImplicitMap }
                  in loc { _locationActionManagement = updatedActionMgmt }
              handleEffect _ = throwError "UndefinedEffect"
        process actionEffectKey@(ObjectKey oid) = do
          case Data.Map.Strict.lookup actionEffectKey actionEffectMap of
            Nothing -> throwError "No effect for actionEffectKey found in actionEffectMap"
            Just effects -> mapM_ handleEffect effects
            where
              handleEffect :: Effect -> GameComputation Identity ()
              handleEffect (DirectionalStimulusEffect directionalStimulusVerb changeTo) = do
                modifyObjectActionManagementM oid $ \actionMgmt ->
                  let directionalMap = _directionalStimulusActionManagement actionMgmt
                      updatedDirectionalMap = Data.Map.Strict.insert directionalStimulusVerb changeTo directionalMap
                  in actionMgmt { _directionalStimulusActionManagement = updatedDirectionalMap }
              handleEffect (ImplicitStimulusEffect implicitStimulusVerb changeTo) = do
                modifyObjectActionManagementM oid $ \actionMgmt ->
                  let implicitMap = _implicitStimulusActionManagement actionMgmt
                      updatedImplicitMap = Data.Map.Strict.insert implicitStimulusVerb changeTo implicitMap
                  in actionMgmt { _implicitStimulusActionManagement = updatedImplicitMap }
              handleEffect (SomaticAccessEffect somaticAccessVerb changeTo) = do
                modifyObjectActionManagementM oid $ \actionMgmt ->
                  let somaticMap = _somaticStimulusActionManagement actionMgmt
                      updatedSomaticMap = Data.Map.Strict.insert somaticAccessVerb changeTo somaticMap
                  in actionMgmt { _somaticStimulusActionManagement = updatedSomaticMap }
              handleEffect (AcquisitionEffect _ _) = do
                -- Acquisition actions are not handled here, they are processed in the main game loop
                return ()
        process _ = modifyNarration (updateActionConsequence "ActionEffectKey unimplemented")

msg :: Text
msg = "You open your eyes, and the world comes into focus."
