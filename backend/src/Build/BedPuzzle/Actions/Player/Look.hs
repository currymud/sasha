{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module Build.BedPuzzle.Actions.Player.Look (
                              dsvActionEnabled
                             , dsvContainerActionEnabled
                             , isvActionEnabled
                             ) where

import           Control.Monad.Reader.Class (asks)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict
import           GameState                  (getObjectM)
import           GameState.ActionManagement (lookupDirectionalContainerStimulus,
                                             lookupDirectionalStimulus,
                                             lookupImplicitStimulus)
import           Model.GameState            (ActionMaps (_directionalStimulusActionMap, _directionalStimulusContainerActionMap, _implicitStimulusActionMap),
                                             Config (_actionMaps),
                                             DirectionalStimulusActionF (DirectionalStimulusActionF),
                                             DirectionalStimulusContainerActionF (DirectionalStimulusContainerActionF),
                                             ImplicitStimulusActionF (ImplicitStimulusActionF),
                                             Location (_locationActionManagement),
                                             Object (_objectActionManagement))
import           Model.GID                  (GID)
import           Model.Parser.Atomics.Verbs (DirectionalStimulusVerb,
                                             ImplicitStimulusVerb)

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

dsvActionEnabled :: DirectionalStimulusVerb ->  DirectionalStimulusActionF
dsvActionEnabled dsv = DirectionalStimulusActionF actionEnabled
  where
    actionEnabled dsnp oid = do
      actionMgmt <- _objectActionManagement <$> getObjectM oid
      case lookupDirectionalStimulus dsv actionMgmt of
        Nothing -> error "Programmer Error: No directional stimulus action found for verb: "
        Just actionGID -> do
          actionMap' :: Map (GID DirectionalStimulusActionF) DirectionalStimulusActionF <- asks (_directionalStimulusActionMap . _actionMaps)
          case Data.Map.Strict.lookup actionGID actionMap' of
            Nothing -> error "Programmer Error: No directional stimulus action found for verb: "
            Just (DirectionalStimulusActionF actionFunc) -> actionFunc dsnp oid

dsvContainerActionEnabled :: DirectionalStimulusVerb ->  DirectionalStimulusContainerActionF
dsvContainerActionEnabled dsv = DirectionalStimulusContainerActionF actionEnabled
  where
    actionEnabled oid = do
      actionMgmt <- _objectActionManagement <$> getObjectM oid
      case lookupDirectionalContainerStimulus dsv actionMgmt of
        Nothing -> error "Programmer Error: No directional stimulus action found for verb: "
        Just actionGID -> do
          actionMap' :: Map (GID DirectionalStimulusContainerActionF) DirectionalStimulusContainerActionF <- asks (_directionalStimulusContainerActionMap . _actionMaps)
          case Data.Map.Strict.lookup actionGID actionMap' of
            Nothing -> error "Programmer Error: No directional stimulus action found for verb: "
            Just (DirectionalStimulusContainerActionF actionFunc) -> actionFunc oid
