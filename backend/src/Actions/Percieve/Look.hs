module Actions.Percieve.Look (actionEnabled,agentCanSee,agentCannotSee, manageImplicitStimulusProcess, manageDirectionalStimulusProcess) where

import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader.Class    (asks)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict
import           Data.Text                     (Text)
import           GameState                     (getPlayerActionsM, getPlayerM,
                                                modifyNarration)
import           GHC.RTS.Flags                 (ProfFlags (doHeapProfile))
import           Location                      (getPlayerLocationM)
import           Model.GameState               (ActionManagement (_directionalStimulusActionManagement, _implicitStimulusActionManagement),
                                                ActionMaps (_directionalStimulusActionMap, _implicitStimulusActionMap),
                                                Config (_actionMaps),
                                                GameComputation,
                                                ImplicitStimulusActionF (ImplicitStimulusActionF, _implicitStimulusAction),
                                                ImplicitStimulusActionMap,
                                                Location (_locationActionManagement, _title),
                                                Player (_location),
                                                PlayerActions (_implicitStimulusActions),
                                                PlayerProcessImplicitVerbMap,
                                                PlayerSentenceProcessingMaps (PlayerSentenceProcessingMaps, _playerProcessImplicitVerbMap),
                                                ProcessDirectionalStimulusVerb (ProcessDirectionalStimulusVerb, _unProcessDirectionalStimlusVerb),
                                                ProcessImplicitStimulusVerb (ProcessImplicitStimulusVerb),
                                                ProcessImplicitVerbMap,
                                                ProcessImplicitVerbMaps,
                                                SentenceProcessingMaps (_processImplicitVerbMap),
                                                updateActionConsequence)
import           Model.GID                     (GID)
import           Model.Parser.Atomics.Verbs    (DirectionalStimulusVerb,
                                                ImplicitStimulusVerb)
import           Model.Parser.Composites.Nouns (DirectionalStimulusNounPhrase)
import           Model.Parser.GCase            (VerbKey (ImplicitStimulusKey))
import           Relude.String.Conversion      (ToText (toText))

agentCanSee :: ImplicitStimulusActionF
agentCanSee = ImplicitStimulusActionF $
   modifyNarration $ updateActionConsequence ("You see: " )

agentCannotSee :: Text -> ImplicitStimulusActionF
agentCannotSee nosee = ImplicitStimulusActionF $
  modifyNarration $ updateActionConsequence nosee

actionEnabled :: ImplicitStimulusVerb -> ImplicitStimulusActionF
actionEnabled isv = ImplicitStimulusActionF $ do
  loc <- getPlayerLocationM
  let actionMap = _implicitStimulusActionManagement $ _locationActionManagement loc
  case Data.Map.Strict.lookup isv actionMap of
    Nothing -> error $ "Programmer Error: No implicit stimulus action found for verb: "
    Just (actionGID :: GID ImplicitStimulusActionF) -> do
      actionMap' ::  Map (GID ImplicitStimulusActionF) ImplicitStimulusActionF <- asks (_implicitStimulusActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap' of
        Nothing -> error $ "Programmer Error: No implicit stimulus action found for verb: "
        Just (ImplicitStimulusActionF actionFunc) -> actionFunc

manageImplicitStimulusProcess :: ImplicitStimulusVerb -> GameComputation Identity ()
manageImplicitStimulusProcess isv = do
  availableActions <- _implicitStimulusActions <$> getPlayerActionsM
  case Data.Map.Strict.lookup isv availableActions of
    Nothing -> error $ "Programmer Error: No implicit stimulus action found for verb: "
    Just (actionGID :: GID ImplicitStimulusActionF) -> do
      actionMap :: ImplicitStimulusActionMap <- asks (_implicitStimulusActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error $ "Programmer Error: No implicit stimulus action found for GID: "
        Just (ImplicitStimulusActionF actionFunc) -> actionFunc
      pure ()

manageDirectionalStimulusProcess :: ProcessDirectionalStimulusVerb
manageDirectionalStimulusProcess = ProcessDirectionalStimulusVerb go
  where
    go :: DirectionalStimulusVerb -> DirectionalStimulusNounPhrase -> GameComputation Identity ()
    go _  _ = pure ()
