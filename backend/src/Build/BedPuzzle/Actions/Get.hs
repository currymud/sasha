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
      {-
get :: AcquisitionActionF
get = AcquisitionActionF getit
  where
-- Data.Bifunctor.second ObjectKey
    getit :: Location -> ActionEffectMap -> AcquisitionVerbPhrase -> GameComputation Identity ()
    getit loc (ActionEffectMap actionEffectMap) avp = do
      let (adj,obj :: NounKey) = Data.Bifunctor.second ObjectiveKey $ case avp of
                        SimpleAcquisitionVerbPhrase _ ophrase ->
                          case ophrase of
                            (ObjectPhrase (SimpleNounPhrase obj')) -> (Nothing,obj')
                            (ObjectPhrase (NounPhrase _ obj')) -> (Nothing,obj')
                            (ObjectPhrase (DescriptiveNounPhrase adj' obj')) -> (Just adj',obj')
                            (ObjectPhrase (DescriptiveNounPhraseDet _ adj' obj')) -> (Just adj',obj')
                        _ -> error "get: unsupported AcquisitionVerbPhrase"

      -- we're not doing disambiguation yet
      oid <- case Data.Map.Strict.lookup obj loc._objectSemanticMap of
        Just o  ->  if Data.Set.null o
                    then error $ "check player inventory not implemented: " <> show obj
                    else pure $ Data.Set.elemAt 0 o
        Nothing -> error $ "get: object not found: check player inv " <> show obj
      pure ()
      -}

parseAcquisitionPhrase :: AcquisitionVerbPhrase -> (ObjectPhrase,NounKey)
parseAcquisitionPhrase avp = Data.Bifunctor.second ObjectiveKey $ case avp of
  SimpleAcquisitionVerbPhrase _ ophrase ->
    let obj = case ophrase of
               (ObjectPhrase (SimpleNounPhrase obj'))             -> obj'
               (ObjectPhrase (NounPhrase _ obj'))                 -> obj'
               (ObjectPhrase (DescriptiveNounPhrase _ obj'))      -> obj'
               (ObjectPhrase (DescriptiveNounPhraseDet _ _ obj')) -> obj'
    in (ophrase, obj)
  _ -> error "get: unsupported AcquisitionVerbPhrase"

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

          -- Get location removal function (returns success or fail action)
--          removeAction <- getLocationRemovalFunctionM loc nounKey

          -- Get object acquisition function (returns success or fail action)
--          addAction <- getObjectAcquisitionFunctionM oid avp

          -- Execute both actions in sequence
--          removeAction
--          addAction
          pure ()
        _ -> modifyNarration $ updateActionConsequence "You don't see that here."

