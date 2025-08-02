module Build.BedPuzzle.Actions.Open where
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.Identity    (Identity)
import qualified Data.Map.Strict
import           Data.Maybe                (catMaybes)
import           Data.Set                  (Set, toList)
import           Data.Text                 (Text)
import           GameState                 (getLocationM, modifyLocationM,
                                            modifyNarration,
                                            modifyObjectActionManagementM,
                                            updatePerceptionMapM)
import           Model.GameState           (ActionEffectKey (LocationKey, ObjectKey),
                                            ActionEffectMap (ActionEffectMap),
                                            ActionManagement (_directionalStimulusActionManagement, _implicitStimulusActionManagement, _somaticStimulusActionManagement),
                                            Effect (DirectionalStimulusEffect, ImplicitStimulusEffect, SomaticAccessEffect),
                                            GameComputation,
                                            Location (_locationActionManagement),
                                            SomaticAccessActionF (SomaticAccessActionF),
                                            updateActionConsequence)


openEyesDenied :: SomaticAccessActionF
openEyesDenied = SomaticAccessActionF (const(const denied))
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
      oids <- catMaybes <$> mapM process (Data.Set.toList actionEffectKeys)
      modifyNarration (updateActionConsequence msg)
      where
        process :: ActionEffectKey -> GameComputation Identity (Maybe ActionEffectKey)
        process actionEffectKey@(LocationKey lid) = do
          case Data.Map.Strict.lookup actionEffectKey actionEffectMap of
            Nothing -> throwError "No effect for actionEffectKey found in actionEffectMap"
            Just effects -> mapM_ handleEffect effects >> pure Nothing
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
            Just effects -> mapM_ handleEffect effects >> pure (Just actionEffectKey)
            where
              handleEffect :: Effect -> GameComputation Identity ()
              handleEffect (DirectionalStimulusEffect directionalStimulusVerb changeTo) = do
                modifyObjectActionManagementM oid $ \actionMgmt ->
                  let directionalMap = _directionalStimulusActionManagement actionMgmt
                      updatedDirectionalMap = Data.Map.Strict.insert directionalStimulusVerb changeTo directionalMap
                  in actionMgmt { _directionalStimulusActionManagement = updatedDirectionalMap }
                updatePerceptionMapM oid
              handleEffect (ImplicitStimulusEffect implicitStimulusVerb changeTo) = do
                modifyObjectActionManagementM oid $ \actionMgmt ->
                  let implicitMap = _implicitStimulusActionManagement actionMgmt
                      updatedImplicitMap = Data.Map.Strict.insert implicitStimulusVerb changeTo implicitMap
                  in actionMgmt { _implicitStimulusActionManagement = updatedImplicitMap }
                updatePerceptionMapM oid
              handleEffect (SomaticAccessEffect somaticAccessVerb changeTo) = do
                modifyObjectActionManagementM oid $ \actionMgmt ->
                  let somaticMap = _somaticStimulusActionManagement actionMgmt
                      updatedSomaticMap = Data.Map.Strict.insert somaticAccessVerb changeTo somaticMap
                  in actionMgmt { _somaticStimulusActionManagement = updatedSomaticMap }
                updatePerceptionMapM oid
        process _ = modifyNarration (updateActionConsequence "ActionEffectKey unimplemented") >> pure Nothing
--          modifyNarration (updateActionConsequence msg) -- changeImplicit look aid >> modifyNarration (updateActionConsequence msg)
msg :: Text
msg = "You open your eyes, and the world comes into focus."
