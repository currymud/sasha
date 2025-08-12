module Build.BedPuzzle.Actions.Stand (standDenied, standUp) where
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.Identity    (Identity)
import qualified Data.Map.Strict
import           Data.Set                  (Set, filter, insert, toList)
import           Data.Text                 (Text, pack)
import           GameState                 (modifyLocationM, modifyNarration,
                                            modifyObjectActionManagementM,
                                            processAcquisitionEffect)
import           GameState.Perception      (buildPerceptionMapFromObjects,
                                            computePerceivableObjects,
                                            modifyPerceptionMapM, youSeeM)
import           Model.GameState           (ActionEffectKey (LocationKey, ObjectKey, PlayerKey),
                                            ActionEffectMap (ActionEffectMap),
                                            ActionManagement (AAManagementKey, DSAManagementKey, ISAManagementKey, NPManagementKey, PPManagementKey, SSAManagementKey),
                                            ActionManagementFunctions (ActionManagementFunctions),
                                            Effect (AcquisitionEffect, DirectionalStimulusEffect, ImplicitStimulusEffect, NegativePosturalEffect, PerceptionEffect, PositivePosturalEffect, SomaticAccessEffect),
                                            GameComputation,
                                            Location (_locationActionManagement),
                                            PosturalActionF (PosturalActionF),
                                            updateActionConsequence)

standDenied :: PosturalActionF
standDenied = PosturalActionF (const (const denied))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You try to stand but the room starts spinning and you lay back down. There's some aspirin in your robe pocket."

standUp :: PosturalActionF
standUp = PosturalActionF stood
  where
    stood :: Set ActionEffectKey -> ActionEffectMap -> GameComputation Identity ()
    stood actionEffectKeys (ActionEffectMap actionEffectMap) = do
      mapM_ process (Data.Set.toList actionEffectKeys)
      modifyNarration (updateActionConsequence msg)
      where
        process :: ActionEffectKey -> GameComputation Identity ()
        process actionEffectKey@(LocationKey lid) = do
          case Data.Map.Strict.lookup actionEffectKey actionEffectMap of
            Nothing -> pure ()
            Just effects -> do
                              modifyNarration (updateActionConsequence "you stand up")
                              mapM_ handleEffect effects
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
              handleEffect (DirectionalStimulusEffect directionalStimulusVerb changeTo) = do
                modifyLocationM lid $ \loc ->
                  let ActionManagementFunctions actionSet = _locationActionManagement loc
                      filteredActions = Data.Set.filter (\case DSAManagementKey v _ -> v /= directionalStimulusVerb; _ -> True) actionSet
                      updatedActions = Data.Set.insert (DSAManagementKey directionalStimulusVerb changeTo) filteredActions
                  in loc { _locationActionManagement = ActionManagementFunctions updatedActions }
              handleEffect (SomaticAccessEffect somaticAccessVerb changeTo) = do
                modifyLocationM lid $ \loc ->
                  let ActionManagementFunctions actionSet = _locationActionManagement loc
                      filteredActions = Data.Set.filter (\case SSAManagementKey v _ -> v /= somaticAccessVerb; _ -> True) actionSet
                      updatedActions = Data.Set.insert (SSAManagementKey somaticAccessVerb changeTo) filteredActions
                  in loc { _locationActionManagement = ActionManagementFunctions updatedActions }
              handleEffect (PositivePosturalEffect verb changeTo) = do
                modifyLocationM lid $ \loc ->
                  let ActionManagementFunctions actionSet = _locationActionManagement loc
                      filteredActions = Data.Set.filter (\case PPManagementKey v _ -> v /= verb; _ -> True) actionSet
                      updatedActions = Data.Set.insert (PPManagementKey verb changeTo) filteredActions
                  in loc { _locationActionManagement = ActionManagementFunctions updatedActions }
              handleEffect (NegativePosturalEffect verb changeTo) = do
                modifyLocationM lid $ \loc ->
                  let ActionManagementFunctions actionSet = _locationActionManagement loc
                      filteredActions = Data.Set.filter (\case NPManagementKey v _ -> v /= verb; _ -> True) actionSet
                      updatedActions = Data.Set.insert (NPManagementKey verb changeTo) filteredActions
                  in loc { _locationActionManagement = ActionManagementFunctions updatedActions }
              handleEffect PerceptionEffect = do
                perceivableObjects <- computePerceivableObjects
                newPerceptionMap <- buildPerceptionMapFromObjects (Data.Set.toList perceivableObjects)
                modifyPerceptionMapM (const newPerceptionMap)
              handleEffect err = throwError (Data.Text.pack $ "Unhandled effect in standUp: " <> show err)
        process actionEffectKey@(ObjectKey oid) = do
          case Data.Map.Strict.lookup actionEffectKey actionEffectMap of
            Nothing      -> pure () -- throwError "No effect for actionEffectKey found in actionEffectMap"
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
              handleEffect (PositivePosturalEffect verb changeTo) = do
                modifyObjectActionManagementM oid $ \actionMgmt ->
                  let ActionManagementFunctions actionSet = actionMgmt
                      filteredActions = Data.Set.filter (\case PPManagementKey v _ -> v /= verb; _ -> True) actionSet
                      updatedActions = Data.Set.insert (PPManagementKey verb changeTo) filteredActions
                  in ActionManagementFunctions updatedActions
              handleEffect (NegativePosturalEffect verb changeTo) = do
                modifyObjectActionManagementM oid $ \actionMgmt ->
                  let ActionManagementFunctions actionSet = actionMgmt
                      filteredActions = Data.Set.filter (\case NPManagementKey v _ -> v /= verb; _ -> True) actionSet
                      updatedActions = Data.Set.insert (NPManagementKey verb changeTo) filteredActions
                  in ActionManagementFunctions updatedActions
              handleEffect (AcquisitionEffect acquisitionVerbPhrase newActionGID) = do
                modifyObjectActionManagementM oid $ \actionMgmt ->
                  let ActionManagementFunctions actionSet = actionMgmt
                      -- Remove any existing acquisition action for this phrase
                      filteredActions = Data.Set.filter (\case AAManagementKey p _ -> p /= acquisitionVerbPhrase; _ -> True) actionSet
                      -- Add new action
                      updatedActions = Data.Set.insert (AAManagementKey acquisitionVerbPhrase newActionGID) filteredActions
                  in ActionManagementFunctions updatedActions
              handleEffect PerceptionEffect = do
                perceivableObjects <- computePerceivableObjects
                newPerceptionMap <- buildPerceptionMapFromObjects (Data.Set.toList perceivableObjects)
                modifyPerceptionMapM (const newPerceptionMap)
              handleEffect err = throwError (Data.Text.pack $ "Unhandled effect in standUp: " <> show err)
        process actionEffectKey@(PlayerKey _) = do
          case Data.Map.Strict.lookup actionEffectKey actionEffectMap of
            Nothing      -> pure ()
            Just effects -> mapM_ handleEffect effects
            where
              handleEffect :: Effect -> GameComputation Identity ()
              handleEffect (AcquisitionEffect targetAVP newActionGID) = do
                processAcquisitionEffect targetAVP newActionGID
              handleEffect err = throwError (Data.Text.pack $ "Unhandled player effect in standUp: " <> show err)
        process _ = modifyNarration (updateActionConsequence "ActionEffectKey unimplemented")

msg :: Text
msg = "You stand up, feeling more alert and ready for action."

standUpDenied :: PosturalActionF
standUpDenied = PosturalActionF (const (const denied))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You're already standing. No need to stand up again."
