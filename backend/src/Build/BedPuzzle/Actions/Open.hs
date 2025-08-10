{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Open where
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.Identity    (Identity)
import qualified Data.Map.Strict
import           Data.Set                  (Set, filter, insert, toList)
import           Data.Text                 (Text)
import           GameState                 (modifyLocationM, modifyNarration,
                                            modifyObjectActionManagementM)
import           GameState.Perception      (buildPerceptionMapFromObjects,
                                            computePerceivableObjects,
                                            modifyPerceptionMapM, youSeeM)
import           Model.GameState           (ActionEffectKey (LocationKey, ObjectKey),
                                            ActionEffectMap (ActionEffectMap),
                                            ActionManagement (DSAManagementKey, ISAManagementKey, SSAManagementKey),
                                            ActionManagementFunctions (ActionManagementFunctions),
                                            Effect (DirectionalStimulusEffect, ImplicitStimulusEffect, PerceptionEffect, SomaticAccessEffect),
                                            GameComputation,
                                            Location (_locationActionManagement),
                                            SomaticAccessActionF (SomaticAccessActionF),
                                            updateActionConsequence)

openEyesDenied :: SomaticAccessActionF
openEyesDenied = SomaticAccessActionF (const (const denied))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "They're already open, relax."

-- Build/BedPuzzle/Actions/Open.hs updates

openEyes :: SomaticAccessActionF
openEyes = SomaticAccessActionF opened
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
                  let ActionManagementFunctions actionSet = _locationActionManagement loc
                      -- Remove old implicit stimulus actions for this verb
                      filteredActions = Data.Set.filter (\case ISAManagementKey v _ -> v /= implicitStimulusVerb; _ -> True) actionSet
                      -- Add new action
                      updatedActions = Data.Set.insert (ISAManagementKey implicitStimulusVerb changeTo) filteredActions
                  in loc { _locationActionManagement = ActionManagementFunctions updatedActions }
              handleEffect _ = throwError "UndefinedEffect"
        process actionEffectKey@(ObjectKey oid) = do
          case Data.Map.Strict.lookup actionEffectKey actionEffectMap of
            Nothing -> throwError "No effect for actionEffectKey found in actionEffectMap"
            Just effects -> mapM_ handleEffect effects
            where
              handleEffect :: Effect -> GameComputation Identity ()
              handleEffect (DirectionalStimulusEffect directionalStimulusVerb changeTo) = do
                modifyObjectActionManagementM oid $ \actionMgmt ->
                  let ActionManagementFunctions actionSet = actionMgmt
                      filteredActions = Data.Set.filter (\case DSAManagementKey v _ -> v /= directionalStimulusVerb; _ -> True) actionSet
                      updatedActions = Data.Set.insert (DSAManagementKey directionalStimulusVerb changeTo) filteredActions
                  in ActionManagementFunctions updatedActions
              handleEffect (ImplicitStimulusEffect implicitStimulusVerb changeTo) = do
                modifyObjectActionManagementM oid $ \actionMgmt ->
                  let ActionManagementFunctions actionSet = actionMgmt
                      filteredActions = Data.Set.filter (\case ISAManagementKey v _ -> v /= implicitStimulusVerb; _ -> True) actionSet
                      updatedActions = Data.Set.insert (ISAManagementKey implicitStimulusVerb changeTo) filteredActions
                  in ActionManagementFunctions updatedActions
              handleEffect (SomaticAccessEffect somaticAccessVerb changeTo) = do
                modifyObjectActionManagementM oid $ \actionMgmt ->
                  let ActionManagementFunctions actionSet = actionMgmt
                      filteredActions = Data.Set.filter (\case SSAManagementKey v _ -> v /= somaticAccessVerb; _ -> True) actionSet
                      updatedActions = Data.Set.insert (SSAManagementKey somaticAccessVerb changeTo) filteredActions
                  in ActionManagementFunctions updatedActions
              handleEffect PerceptionEffect = do
              -- Compute what should be perceivable based on current spatial relationships
                perceivableObjects <- computePerceivableObjects
  -- Build the new perception map from those objects
                newPerceptionMap <- buildPerceptionMapFromObjects (Data.Set.toList perceivableObjects)
  -- Replace the entire perception map with the computed one
                modifyPerceptionMapM (const newPerceptionMap)
              handleEffect _ = modifyNarration (updateActionConsequence "handleEffect unimplemented")
        process _ = modifyNarration (updateActionConsequence "ActionEffectKey unimplemented")

msg :: Text
msg = "You open your eyes, and the world comes into focus."
