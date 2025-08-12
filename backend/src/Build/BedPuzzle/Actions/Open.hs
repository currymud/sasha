{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Open where
import           Control.Monad.Error.Class     (throwError)
import           Control.Monad.Identity        (Identity)
import qualified Data.Map.Strict
import           Data.Set                      (Set, filter, insert, toList)
import           Data.Text                     (Text, pack)
import           GameState                     (modifyLocationM,
                                                modifyNarration,
                                                modifyObjectActionManagementM)
import           GameState.Perception          (buildPerceptionMapFromObjects,
                                                computePerceivableObjects,
                                                modifyPerceptionMapM, youSeeM)
import           Model.GameState               (ActionEffectKey (LocationKey, ObjectKey),
                                                ActionEffectMap (ActionEffectMap),
                                                ActionManagement (CAManagementKey, DSAManagementKey, ISAManagementKey, NPManagementKey, PPManagementKey, SSAManagementKey),
                                                ActionManagementFunctions (ActionManagementFunctions),
                                                Effect (AcquisitionEffect, ConsumptionEffect, DirectionalStimulusEffect, ImplicitStimulusEffect, NegativePosturalEffect, PerceptionEffect, PositivePosturalEffect, SomaticAccessEffect),
                                                GameComputation,
                                                Location (_locationActionManagement),
                                                SomaticAccessActionF (SomaticAccessActionF),
                                                updateActionConsequence)
import           Model.Parser.Composites.Verbs (ConsumptionVerbPhrase (ConsumptionVerbPhrase))

openEyesDenied :: SomaticAccessActionF
openEyesDenied = SomaticAccessActionF (const (const denied))
 where
   denied :: GameComputation Identity ()
   denied = modifyNarration $ updateActionConsequence msg
   msg :: Text
   msg = "They're already open, relax."

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
           Nothing      -> pure () -- throwError "No effect for actionEffectKey found in actionEffectMap"
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
             handleEffect (AcquisitionEffect acquisitionVerb changeTo) = do
               modifyNarration (updateActionConsequence "AcquisitionEffect handled")
             handleEffect err = throwError (Data.Text.pack $ "OUCH" <> show err)
       process actionEffectKey@(ObjectKey oid) = do
         case Data.Map.Strict.lookup actionEffectKey actionEffectMap of
           Nothing      -> pure () --  throwError "No effect for actionEffectKey found in actionEffectMap"
           Just effects -> mapM_ handleEffect effects
           where
             handleEffect :: Effect -> GameComputation Identity ()
             handleEffect (ConsumptionEffect consumptionVerb targetOid changeTo) = do
               modifyObjectActionManagementM targetOid $ \actionMgmt ->
                 let ActionManagementFunctions actionSet = actionMgmt
                     -- Remove old consumption actions for this verb
                     filteredActions = Data.Set.filter filterAction actionSet
                     -- Find existing consumption verb phrase to preserve
                     existingCVP = case [cvp | CAManagementKey cvp@(ConsumptionVerbPhrase verb _) _ <- Data.Set.toList actionSet, verb == consumptionVerb] of
                       (foundCvp:_) -> foundCvp
                       []           -> error "No existing consumption verb phrase found for effect"
                     -- Add new action with existing phrase
                     updatedActions = Data.Set.insert (CAManagementKey existingCVP changeTo) filteredActions
                 in ActionManagementFunctions updatedActions
               where
                 filterAction (CAManagementKey (ConsumptionVerbPhrase verb _) _) = verb /= consumptionVerb
                 filterAction _ = True
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
             handleEffect PerceptionEffect = do
               perceivableObjects <- computePerceivableObjects
               newPerceptionMap <- buildPerceptionMapFromObjects (Data.Set.toList perceivableObjects)
               modifyPerceptionMapM (const newPerceptionMap)
             handleEffect (AcquisitionEffect acquisitionVerb changeTo) = do
               modifyNarration (updateActionConsequence "AcquisitionEffect handled")
       process _ = modifyNarration (updateActionConsequence "ActionEffectKey unimplemented")

msg :: Text
msg = "You open your eyes, and the world comes into focus."
