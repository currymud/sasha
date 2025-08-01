module Build.BedPuzzle.Actions.Open where
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.Identity    (Identity)
import qualified Data.Map.Strict
import           Data.Set                  (Set, toList)
import           Data.Text                 (Text)
import           GameState                 (getLocationM, modifyLocationM,
                                            modifyNarration)
import           Model.GameState           (ActionEffectKey (LocationKey),
                                            ActionEffectMap (ActionEffectMap),
                                            Effect (ImplicitStimulusEffect),
                                            GameComputation,
                                            Location (_locationActionManagement),
                                            SomaticAccessActionF (SomaticAccessActionF),
                                            _implicitStimulusActionManagement,
                                            updateActionConsequence)


  {-
I need maps of GIDs to actions, to tell me how to handle
effects of opening eyes
     -}
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
      mapM_ process (Data.Set.toList actionEffectKeys)
      where
        process :: ActionEffectKey -> GameComputation Identity ()
        process actionEffectKey@(LocationKey lid) = do
          case Data.Map.Strict.lookup actionEffectKey actionEffectMap of
            Nothing -> throwError "No effect for actionEffectKey found in actionEffectMap"
            Just effects -> mapM_ handleEffect effects
            where
              handleEffect :: Effect -> GameComputation Identity ()
              handleEffect (ImplicitStimulusEffect implicitStimulusVerb changeTo) = do
  -- Use the lid from the outer scope to modify the location's action management
                modifyLocationM lid $ \loc ->
                  let actionMgmt = _locationActionManagement loc
                      implicitMap = _implicitStimulusActionManagement actionMgmt
                      updatedImplicitMap = Data.Map.Strict.insert implicitStimulusVerb changeTo implicitMap
                      updatedActionMgmt = actionMgmt { _implicitStimulusActionManagement = updatedImplicitMap }
                  in loc { _locationActionManagement = updatedActionMgmt }
                modifyNarration (updateActionConsequence msg)
              handleEffect _ = throwError "UndefinedEffect"
        process _ = throwError "ActionEffectKey unimplemented"
--          modifyNarration (updateActionConsequence msg) -- changeImplicit look aid >> modifyNarration (updateActionConsequence msg)
msg :: Text
msg = "You open your eyes, and the world comes into focus."
