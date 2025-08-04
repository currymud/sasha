{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Get (get,getDenied) where
import           Control.Monad.Error.Class     (throwError)
import           Control.Monad.Identity        (Identity)
import qualified Data.Map.Strict
import           Data.Maybe                    (catMaybes)
import           Data.Set                      (Set, fromList, toList)
import           Data.Text                     (Text)
import           GameState                     (getLocationM, modifyLocationM,
                                                modifyNarration,
                                                modifyObjectActionManagementM,
                                                updatePerceptionMapM, youSeeM)
import           Model.GameState               (AcquisitionActionF (AcquisitionActionF),
                                                ActionEffectKey (LocationKey, ObjectKey),
                                                ActionEffectMap (ActionEffectMap),
                                                ActionManagement (_directionalStimulusActionManagement, _implicitStimulusActionManagement, _somaticStimulusActionManagement),
                                                Effect (AcquisitionEffect, DirectionalStimulusEffect, ImplicitStimulusEffect, SomaticAccessEffect),
                                                GameComputation,
                                                Location (_locationActionManagement),
                                                Object, updateActionConsequence)
import           Model.GID                     (GID)
import           Model.Parser.Atomics.Nouns    (Objective (Objective))
import           Model.Parser.Composites.Nouns (NounPhrase (SimpleNounPhrase),
                                                ObjectPhrase (ObjectPhrase))
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase (AcquisitionVerbPhrase, SimpleAcquisitionVerbPhrase))


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
    getit loc (ActionEffectMap actionEffectMap) avp = do
      obj <- case avp of
               SimpleAcquisitionVerbPhrase _ ophrase ->
                 case ophrase of
                   (ObjectPhrase (SimpleNounPhrase obj)) -> pure ()

                   _ -> throwError "get: SimpleNounPhrase expected"
               _ -> throwError "get: SimpleAcquisitionVerbPhrase expected"
      pure ()
