{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module Actions.Percieve.Look ( lookAt
                             , dsvActionEnabled
                             , isvActionEnabled
                             , agentCanSee
                             , agentCannotSee
                             , manageImplicitStimulusProcess
                             , manageDirectionalStimulusProcess
                             ) where

import           Control.Monad.Identity                                  (Identity)
import           Control.Monad.Reader.Class                              (asks)
import           Data.Map.Strict                                         (Map)
import qualified Data.Map.Strict
import qualified Data.Set
import           Data.Text                                               (Text)
import           GameState                                               (getObjectM,
                                                                          getPlayerM,
                                                                          modifyNarration)
import           GameState.ActionManagement                              (lookupDirectionalStimulus,
                                                                          lookupImplicitStimulus)
import           GameState.Perception                                    (findAccessibleObject,
                                                                          queryPerceptionMap)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import           Location                                                (getLocationM,
                                                                          getPlayerLocationM)
import           Model.GameState                                         (ActionMaps (_directionalStimulusActionMap, _implicitStimulusActionMap),
                                                                          Config (_actionMaps),
                                                                          DirectionalStimulusActionF (DirectionalStimulusActionF),
                                                                          GameComputation,
                                                                          ImplicitStimulusActionF (ImplicitStimulusActionF),
                                                                          ImplicitStimulusActionMap,
                                                                          Location (_locationActionManagement, _objectSemanticMap, _title),
                                                                          Object (_descriptives, _objectActionManagement),
                                                                          Player (_location, _playerActions),
                                                                          updateActionConsequence)
import           Model.GID                                               (GID)
import           Model.Parser.Atomics.Nouns                              (DirectionalStimulus)
import           Model.Parser.Atomics.Verbs                              (DirectionalStimulusVerb,
                                                                          ImplicitStimulusVerb)
import           Model.Parser.Composites.Nouns                           (DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                                          NounPhrase (DescriptiveNounPhrase, DescriptiveNounPhraseDet, NounPhrase, SimpleNounPhrase))
import           Model.Parser.GCase                                      (NounKey (DirectionalStimulusKey))
import           Relude.String.Conversion                                (ToText (toText))

agentCanSee :: ImplicitStimulusActionF
agentCanSee = ImplicitStimulusActionF $ const (\loc -> modifyNarration $ updateActionConsequence ("You see: " <> toText (_title loc)))

agentCannotSee :: Text -> ImplicitStimulusActionF
agentCannotSee nosee = ImplicitStimulusActionF
  $ const (const (modifyNarration $ updateActionConsequence nosee))

isvActionEnabled :: ImplicitStimulusVerb -> ImplicitStimulusActionF
isvActionEnabled isv = ImplicitStimulusActionF actionEnabled
  where
    actionEnabled player loc = do
      let actionMgmt = _locationActionManagement loc
      case lookupImplicitStimulus isv actionMgmt of
        Nothing -> error "Programmer Error: No implicit stimulus action found for verb: "
        Just actionGID -> do
          actionMap' :: Map (GID ImplicitStimulusActionF) ImplicitStimulusActionF <- asks (_implicitStimulusActionMap . _actionMaps)
          case Data.Map.Strict.lookup actionGID actionMap' of
            Nothing -> error "Programmer Error: No implicit stimulus action found for verb: "
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

lookAt :: DirectionalStimulusActionF
lookAt = DirectionalStimulusActionF lookAt'
  where
    lookAt' :: DirectionalStimulusNounPhrase -> GID Object -> GameComputation Identity ()
    lookAt' dsnp oid = do
          actionMgmt <- _objectActionManagement <$> getObjectM oid
          case lookupDirectionalStimulus look actionMgmt of
            Nothing -> modifyNarration $ updateActionConsequence "Programmer made a thing you can't look at"
            Just dsaGID -> do
              dsActionMap' <- asks (_directionalStimulusActionMap . _actionMaps)
              case Data.Map.Strict.lookup dsaGID dsActionMap' of
                Nothing -> modifyNarration $ updateActionConsequence "Programmer made a key to an action that can't be found"
                Just (DirectionalStimulusActionF actionFunc) -> actionFunc dsnp oid

manageImplicitStimulusProcess :: ImplicitStimulusVerb -> GameComputation Identity ()
manageImplicitStimulusProcess isv = do
  availableActions <- _playerActions <$> getPlayerM
  case lookupImplicitStimulus isv availableActions of
    Nothing -> error "Programmer Error: No implicit stimulus action found for verb: "
    Just actionGID -> do
      actionMap :: ImplicitStimulusActionMap <- asks (_implicitStimulusActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No implicit stimulus action found for GID: "
        Just (ImplicitStimulusActionF actionFunc) -> do
          player <- getPlayerM
          let lid = player._location
          loc <- getLocationM lid
          actionFunc player loc


manageDirectionalStimulusProcess :: DirectionalStimulusVerb -> DirectionalStimulusNounPhrase -> GameComputation Identity ()
manageDirectionalStimulusProcess dsv dsnp = do
  availableActions <- _playerActions <$> getPlayerM
  case lookupDirectionalStimulus dsv availableActions of
    Nothing -> error "Programmer Error: No directional stimulus action found for verb: "
    Just actionGID -> do
      actionMap <- asks (_directionalStimulusActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No directional stimulus action found for GID: "
        Just actionFunc -> do
          location <- getPlayerLocationM
          lookable actionFunc dsnp location


lookable :: DirectionalStimulusActionF
         -> DirectionalStimulusNounPhrase
         -> Location
         -> GameComputation Identity ()
lookable (DirectionalStimulusActionF actionF) dsnp _loc = do
  -- Try direct perception map first
  queryPerceptionMap dsnp >>= \case
    objGIDSet | not (Data.Set.null objGIDSet) -> do
      let firstObjGID = Data.Set.elemAt 0 objGIDSet
      actionF dsnp firstObjGID
    _ -> do
      -- Try accessible objects (containers, surfaces, etc.)
      let noun = extractNoun dsnp
      findAccessibleObject (DirectionalStimulusKey noun) >>= \case
        Just objGID -> actionF dsnp objGID
        Nothing -> modifyNarration $ updateActionConsequence "That's not here. Try something else."

extractNoun :: DirectionalStimulusNounPhrase -> DirectionalStimulus
extractNoun (DirectionalStimulusNounPhrase np) = case np of
  SimpleNounPhrase noun             -> noun
  NounPhrase _ noun                 -> noun
  DescriptiveNounPhrase _ noun      -> noun
  DescriptiveNounPhraseDet _ _ noun -> noun
