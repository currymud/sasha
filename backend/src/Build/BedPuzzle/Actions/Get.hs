{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Get (get,getDenied) where
import           Control.Monad.Identity          (Identity)
import           Control.Monad.Reader            (asks)
import qualified Data.Map.Strict
import           Data.Maybe                      (catMaybes)
import           Data.Set                        (Set, elemAt, fromList, null,
                                                  toList)
import           Data.Text                       (Text)
import           GameState                       (getLocationM, modifyLocationM,
                                                  modifyNarration,
                                                  modifyObjectActionManagementM,
                                                  parseAcquisitionPhrase,
                                                  updatePerceptionMapM, youSeeM)
import           Model.GameState                 (AcquisitionActionF (AcquisitionActionF),
                                                  ActionEffectKey (LocationKey, ObjectKey),
                                                  ActionEffectMap (ActionEffectMap),
                                                  ActionManagement (_acquisitionActionManagement, _directionalStimulusActionManagement, _implicitStimulusActionManagement, _somaticStimulusActionManagement),
                                                  ActionMaps (ActionMaps, _acquisitionActionMap),
                                                  Config (Config, _actionMaps),
                                                  Effect (AcquisitionEffect, DirectionalStimulusEffect, ImplicitStimulusEffect, SomaticAccessEffect),
                                                  GameComputation,
                                                  Location (_locationActionManagement, _objectSemanticMap),
                                                  Object,
                                                  updateActionConsequence)
import           Model.GID                       (GID)
import           Model.Parser.Atomics.Adjectives (Adjective)
import           Model.Parser.Atomics.Nouns      (Objective (Objective))
import           Model.Parser.Composites.Nouns   (NounPhrase (DescriptiveNounPhrase, DescriptiveNounPhraseDet, NounPhrase, SimpleNounPhrase),
                                                  ObjectPhrase (ObjectPhrase))
import           Model.Parser.Composites.Verbs   (AcquisitionVerbPhrase (AcquisitionVerbPhrase))
import           Model.Parser.GCase              (NounKey (ObjectiveKey))


getDenied :: AcquisitionActionF
getDenied = AcquisitionActionF (const (const (const denied)))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You try but feel dizzy and have to lay back down"

get :: AcquisitionActionF
get = AcquisitionActionF getit
  where
    getit :: Location -> ActionEffectMap -> AcquisitionVerbPhrase -> GameComputation Identity ()
    getit loc actionEffectMap avp = do
      let (objectPhrase,nounKey) = parseAcquisitionPhrase avp

      -- Find the object in the current location
      case Data.Map.Strict.lookup nounKey loc._objectSemanticMap of
        Just objSet | not (Data.Set.null objSet) -> do
          let oid = Data.Set.elemAt 0 objSet
          pure ()
        _ -> modifyNarration $ updateActionConsequence "You don't see that here."


executeLocationGet :: Location
                        -> AcquisitionVerbPhrase
                        -> GameComputation Identity (Either (GameComputation Identity ()) (GameComputation Identity ()))
executeLocationGet loc avp = do

  -- Look up location's acquisition actions
  let locationAcquisitionActions = _acquisitionActionManagement (_locationActionManagement loc)

  case Data.Map.Strict.lookup avp locationAcquisitionActions of
    Just actionGID -> do
      -- Get the location action from the action map
      actionMap <- asks (_acquisitionActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Just (AcquisitionActionF locationAction) -> do
          -- Execute location action (removes from location)
          pure $ Left $ locationAction loc (ActionEffectMap mempty) avp
        _ -> pure $ Left $ modifyNarration $ updateActionConsequence  "-- No location-specific action"
    Nothing -> pure $ Left $ modifyNarration $ updateActionConsequence "-- No location-specific action for this acquisition"
