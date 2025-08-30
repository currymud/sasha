{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module Build.BedPuzzle.Actions.Player.Look (
                              dsvActionEnabled
                             , dsvContainerActionEnabled
                             , isvActionEnabled
                             ) where

import           Control.Monad.Reader.Class    (asks)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict
import qualified Data.Set
import           GameState                     (getObjectM, modifyNarration)
import           GameState.ActionManagement    (lookupDirectionalContainerStimulus,
                                                lookupDirectionalStimulus,
                                                lookupImplicitStimulus)
import           GameState.Perception          (findAccessibleObject,
                                                queryPerceptionMap)
import           Model.GameState               (ActionMaps (_directionalStimulusActionMap, _directionalStimulusContainerActionMap, _implicitStimulusActionMap),
                                                Config (_actionMaps),
                                                DirectionalStimulusActionF (DirectionalStimulusActionF),
                                                DirectionalStimulusContainerActionF (DirectionalStimulusContainerActionF),
                                                ImplicitStimulusActionF (ImplicitStimulusActionF),
                                                Location (_locationActionManagement),
                                                Object (_objectActionManagement),
                                                updateActionConsequence)
import           Model.GID                     (GID)
import           Model.Parser.Atomics.Nouns    (Container, DirectionalStimulus)
import           Model.Parser.Atomics.Verbs    (DirectionalStimulusVerb,
                                                ImplicitStimulusVerb)
import           Model.Parser.Composites.Nouns (ContainerPhrase (ContainerPhrase, SimpleContainerPhrase),
                                                DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                NounPhrase (DescriptiveNounPhrase, DescriptiveNounPhraseDet, NounPhrase, SimpleNounPhrase))
import           Model.Parser.GCase            (NounKey (ContainerKey, DirectionalStimulusKey))

isvActionEnabled :: ImplicitStimulusVerb -> ImplicitStimulusActionF
isvActionEnabled isv = ImplicitStimulusActionF actionEnabled
  where
    actionEnabled player loc = do
      let actionMgmt = _locationActionManagement loc
      case lookupImplicitStimulus isv actionMgmt of
        Nothing -> error "Programmer Error: No implicit stimulus action found for verb: in location map"
        Just actionGID -> do
          actionMap' :: Map (GID ImplicitStimulusActionF) ImplicitStimulusActionF <- asks (_implicitStimulusActionMap . _actionMaps)
          case Data.Map.Strict.lookup actionGID actionMap' of
            Nothing -> error "Programmer Error: No implicit stimulus action found for verb: in actionmap "
            Just (ImplicitStimulusActionF actionFunc) -> actionFunc player loc

dsvActionEnabled :: DirectionalStimulusActionF
dsvActionEnabled = DirectionalStimulusActionF actionEnabled
  where
    actionEnabled dsv dsnp = do
      -- Handle availability question here (like getF does)
      -- Try direct perception map first
      queryPerceptionMap dsnp >>= \case
        objGIDSet | not (Data.Set.null objGIDSet) -> do
          let firstObjGID = Data.Set.elemAt 0 objGIDSet
          -- Now get the object's response action and call it
          actionMgmt <- _objectActionManagement <$> getObjectM firstObjGID
          case lookupDirectionalStimulus dsv actionMgmt of
            Nothing -> error "Programmer Error: No directional stimulus action found for verb: "
            Just actionGID -> do
              actionMap' <- asks (_directionalStimulusActionMap . _actionMaps)
              case Data.Map.Strict.lookup actionGID actionMap' of
                Nothing -> error "Programmer Error: No directional stimulus action found for GID: "
                Just (ObjectDirectionalStimulusActionF actionFunc) -> actionFunc dsnp firstObjGID
                Just (PlayerDirectionalStimulusActionF _) -> error "Programmer Error: PlayerDirectionalStimulusActionF found in object action map"
        _ -> do
          -- Try accessible objects (containers, surfaces, etc.)
          let noun = extractNoun dsnp
          findAccessibleObject (DirectionalStimulusKey noun) >>= \case
            Just objGID -> do
              -- Get object's response action and call it
              actionMgmt <- _objectActionManagement <$> getObjectM objGID
              case lookupDirectionalStimulus dsv actionMgmt of
                Nothing -> error "Programmer Error: No directional stimulus action found for verb: "
                Just actionGID -> do
                  actionMap' <- asks (_directionalStimulusActionMap . _actionMaps)
                  case Data.Map.Strict.lookup actionGID actionMap' of
                    Nothing -> error "Programmer Error: No directional stimulus action found for GID: "
                    Just (DirectionalStimulusActionF actionFunc) -> actionFunc dsnp objGID
            Nothing -> modifyNarration $ updateActionConsequence "That's not here. Try something else."

dsvContainerActionEnabled :: DirectionalStimulusVerb
                               -> ContainerPhrase
                               -> DirectionalStimulusContainerActionF
dsvContainerActionEnabled dsv cp = DirectionalStimulusContainerActionF actionEnabled
  where
    actionEnabled _oid = do
      -- This needs to be reworked since we need the ContainerPhrase, not just the oid
      -- The management function should pass the ContainerPhrase to this function
      -- For now, this is the pattern:
      let containerNounKey = extractContainerNoun cp -- Need cp parameter
      findAccessibleObject containerNounKey >>= \case
        Just objGID -> do
          actionMgmt <- _objectActionManagement <$> getObjectM objGID
          case lookupDirectionalContainerStimulus dsv actionMgmt of
            Nothing -> error "Programmer Error: No directional stimulus action found for verb: "
            Just actionGID -> do
              actionMap' <- asks (_directionalStimulusContainerActionMap . _actionMaps)
              case Data.Map.Strict.lookup actionGID actionMap' of
                Nothing -> error "Programmer Error: No directional stimulus action found for GID: "
                Just (DirectionalStimulusContainerActionF actionFunc) -> actionFunc objGID
        Nothing -> modifyNarration $ updateActionConsequence "That container is not here."

extractContainerNoun :: ContainerPhrase -> NounKey
extractContainerNoun cp = case cp of
  (SimpleContainerPhrase nounPhrase) -> extractContainerNounFromPhrase nounPhrase
  (ContainerPhrase _ nounPhrase) -> extractContainerNounFromPhrase nounPhrase
  where
    extractContainerNounFromPhrase :: NounPhrase Container -> NounKey
    extractContainerNounFromPhrase (SimpleNounPhrase container) = ContainerKey container
    extractContainerNounFromPhrase (NounPhrase _ container) = ContainerKey container
    extractContainerNounFromPhrase (DescriptiveNounPhrase _ container) = ContainerKey container
    extractContainerNounFromPhrase (DescriptiveNounPhraseDet _ _ container) = ContainerKey container

extractNoun :: DirectionalStimulusNounPhrase -> DirectionalStimulus
extractNoun (DirectionalStimulusNounPhrase np) = case np of
  SimpleNounPhrase noun             -> noun
  NounPhrase _ noun                 -> noun
  DescriptiveNounPhrase _ noun      -> noun
  DescriptiveNounPhraseDet _ _ noun -> noun
