{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Get (get,getDenied) where
import           Control.Monad.Error.Class       (throwError)
import           Control.Monad.Identity          (Identity)
import qualified Data.Bifunctor
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
                                                  ActionManagement (_directionalStimulusActionManagement, _implicitStimulusActionManagement, _somaticStimulusActionManagement),
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
import           Model.Parser.Composites.Verbs   (AcquisitionVerbPhrase (AcquisitionVerbPhrase, SimpleAcquisitionVerbPhrase))
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

