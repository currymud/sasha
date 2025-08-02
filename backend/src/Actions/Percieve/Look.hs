{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module Actions.Percieve.Look ( lookAt, dsvActionEnabled, isvActionEnabled,agentCanSee,agentCannotSee, manageImplicitStimulusProcess, manageDirectionalStimulusProcess) where

import           Control.Monad.Identity                                  (Identity)
import           Control.Monad.Reader.Class                              (asks)
import           Data.Map.Strict                                         (Map)
import qualified Data.Map.Strict
import           Data.Text                                               (Text)
import           GameState                                               (getActionManagementM,
                                                                          getObjectM,
                                                                          getPlayerActionsM,
                                                                          modifyNarration)
import           GHC.RTS.Flags                                           (ProfFlags (doHeapProfile))
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import           Location                                                (getPlayerLocationM)
import           Model.GameState                                         (ActionManagement (_directionalStimulusActionManagement, _implicitStimulusActionManagement),
                                                                          ActionMaps (_directionalStimulusActionMap, _implicitStimulusActionMap),
                                                                          Config (_actionMaps),
                                                                          DirectionalStimulusActionF (DirectionalStimulusActionF),
                                                                          GameComputation,
                                                                          ImplicitStimulusActionF (ImplicitStimulusActionF, _implicitStimulusAction),
                                                                          ImplicitStimulusActionMap,
                                                                          Location (_locationActionManagement, _objectSemanticMap, _title),
                                                                          Object (_objectActionManagement),
                                                                          PlayerActions (_directionalStimulusActions, _implicitStimulusActions),
                                                                          updateActionConsequence)
import           Model.GID                                               (GID)
import           Model.Parser.Atomics.Nouns                              (DirectionalStimulus (DirectionalStimulus))
import           Model.Parser.Atomics.Verbs                              (DirectionalStimulusVerb,
                                                                          ImplicitStimulusVerb)
import           Model.Parser.Composites.Nouns                           (DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                                          NounPhrase (DescriptiveNounPhrase, DescriptiveNounPhraseDet, NounPhrase, SimpleNounPhrase))
import           Model.Parser.GCase                                      (NounKey (DirectionalStimulusKey))
import           Relude.String.Conversion                                (ToText (toText))

agentCanSee :: ImplicitStimulusActionF
agentCanSee = ImplicitStimulusActionF (\loc -> do
   modifyNarration $ updateActionConsequence ("You see: " <> toText (_title loc)))

agentCannotSee :: Text -> ImplicitStimulusActionF
agentCannotSee nosee = ImplicitStimulusActionF (\_ -> do
  modifyNarration $ updateActionConsequence nosee)

lookAt :: DirectionalStimulusActionF
lookAt = DirectionalStimulusActionF lookAt'
  where
    lookAt' :: DirectionalStimulusNounPhrase -> GID Object -> GameComputation Identity ()
    lookAt' dsnp oid = do
          dsActionMap <- _directionalStimulusActionManagement <$> getActionManagementM oid
          case Data.Map.Strict.lookup look dsActionMap of
            Nothing -> do
              modifyNarration $ updateActionConsequence "Programmer made a thing you can't look at"
            Just dsaGID -> do
              dsActionMap' <- asks (_directionalStimulusActionMap . _actionMaps)
              case Data.Map.Strict.lookup dsaGID dsActionMap' of
                Nothing -> do
                  modifyNarration $ updateActionConsequence "Programmer made a key to an action that can't be found"
                Just (DirectionalStimulusActionF actionFunc) -> do
                  actionFunc dsnp oid

isvActionEnabled :: ImplicitStimulusVerb -> ImplicitStimulusActionF
isvActionEnabled isv = ImplicitStimulusActionF actionEnabled
  where
    actionEnabled loc = do
      let actionMap = _implicitStimulusActionManagement $ _locationActionManagement loc
      case Data.Map.Strict.lookup isv actionMap of
        Nothing -> error $ "Programmer Error: No implicit stimulus action found for verb: "
        Just (actionGID :: GID ImplicitStimulusActionF) -> do
          actionMap' ::  Map (GID ImplicitStimulusActionF) ImplicitStimulusActionF <- asks (_implicitStimulusActionMap . _actionMaps)
          case Data.Map.Strict.lookup actionGID actionMap' of
            Nothing -> error $ "Programmer Error: No implicit stimulus action found for verb: "
            Just (ImplicitStimulusActionF actionFunc) -> actionFunc loc

dsvActionEnabled :: DirectionalStimulusVerb ->  DirectionalStimulusActionF
dsvActionEnabled dsv = DirectionalStimulusActionF actionEnabled
  where
    actionEnabled dsnp oid = do
      actionMap <- _directionalStimulusActionManagement .  _objectActionManagement <$> getObjectM oid
      case Data.Map.Strict.lookup dsv actionMap of
        Nothing -> error $ "Programmer Error: No directional stimulus action found for verb: "
        Just (actionGID :: GID DirectionalStimulusActionF) -> do
          actionMap' :: Map (GID DirectionalStimulusActionF) DirectionalStimulusActionF <- asks (_directionalStimulusActionMap . _actionMaps)
          case Data.Map.Strict.lookup actionGID actionMap' of
            Nothing -> error $ "Programmer Error: No directional stimulus action found for verb: "
            Just (DirectionalStimulusActionF actionFunc) -> actionFunc dsnp oid

manageImplicitStimulusProcess :: ImplicitStimulusVerb -> GameComputation Identity ()
manageImplicitStimulusProcess isv = do
  availableActions <- _implicitStimulusActions <$> getPlayerActionsM
  case Data.Map.Strict.lookup isv availableActions of
    Nothing -> error $ "Programmer Error: No implicit stimulus action found for verb: "
    Just (actionGID :: GID ImplicitStimulusActionF) -> do
      actionMap :: ImplicitStimulusActionMap <- asks (_implicitStimulusActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error $ "Programmer Error: No implicit stimulus action found for GID: "
        Just (ImplicitStimulusActionF actionFunc) -> do
          location <- getPlayerLocationM
          actionFunc location

manageDirectionalStimulusProcess :: DirectionalStimulusVerb -> DirectionalStimulusNounPhrase -> GameComputation Identity ()
manageDirectionalStimulusProcess dsv dsnp = do
  availableActions <- _directionalStimulusActions <$> getPlayerActionsM
  case Data.Map.Strict.lookup dsv availableActions of
    Nothing -> error $ "Programmer Error: No directional stimulus action found for verb: "
    Just (actionGID :: GID DirectionalStimulusActionF) -> do
      actionMap <- asks (_directionalStimulusActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error $ "Programmer Error: No directional stimulus action found for GID: "
        Just actionFunc -> do
          location <- getPlayerLocationM
          lookable actionFunc dsnp location
  where
    lookable (DirectionalStimulusActionF actionF) dsnp@(DirectionalStimulusNounPhrase np)  loc =
      case Data.Map.Strict.lookup nounKey (_objectSemanticMap loc) of
        Nothing -> do
          modifyNarration $ updateActionConsequence "That's not here. Try something else."
        Just objGID -> do
          actionF dsnp objGID
      where
        nounKey = DirectionalStimulusKey dsn'
        dsn' = case np of
               SimpleNounPhrase dsn             -> dsn
               NounPhrase _ dsn                 -> dsn
               DescriptiveNounPhrase  _ dsn     -> dsn
               DescriptiveNounPhraseDet _ _ dsn -> dsn

