module Actions.Percieve.Look (actionEnabled,agentCanSee,agentCannotSee, manageImplicitStimulusProcess, manageDirectionalStimulusProcess) where

import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader.Class    (asks)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict
import           Data.Text                     (Text)
import           GameState                     (getPlayerM, modifyNarration)
import           Location                      (getPlayerLocationM)
import           Model.GameState               (ActionManagement (_directionalStimulusActionManagement, _implicitStimulusActionManagement),
                                                ActionMaps (_directionalStimulusActionMap, _implicitStimulusActionMap),
                                                Config (_actionMaps, _sentenceProcessingMaps),
                                                GameComputation,
                                                ImplicitStimulusActionF (ImplicitStimulusActionF, _implicitStimulusAction),
                                                Location (_locationActionManagement, _title),
                                                Player (_location, _sentenceManagement),
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
    Just (actionGID :: GID ImplicitStimulusVerb) -> do
      actionMap' ::  Map (GID ImplicitStimulusVerb) ImplicitStimulusActionF <- asks (_implicitStimulusActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap' of
        Nothing -> error $ "Programmer Error: No implicit stimulus action found for verb: "
        Just (ImplicitStimulusActionF actionFunc) -> actionFunc

manageImplicitStimulusProcess :: ImplicitStimulusVerb -> GameComputation Identity ()
manageImplicitStimulusProcess isv = do
  spMaps :: ProcessImplicitVerbMaps <- asks (_processImplicitVerbMap . _sentenceProcessingMaps)
  gidMap <- _playerProcessImplicitVerbMap . _sentenceManagement <$> getPlayerM
  case Data.Map.Strict.lookup isv gidMap of
    Nothing -> error $ "Programmer Error: No implicit stimulus verb found for: "
    Just gid -> case Data.Map.Strict.lookup isv spMaps of
      Nothing -> error $ "Programmer Error: No implicit stimulus verb found for: "
      Just amap -> case Data.Map.Strict.lookup gid amap of
         Nothing -> error $ "Programmer Error: No implicit stimulus action found for verb: "
         Just (ProcessImplicitStimulusVerb action) -> action isv

manageDirectionalStimulusProcess :: ProcessDirectionalStimulusVerb
manageDirectionalStimulusProcess = ProcessDirectionalStimulusVerb go
  where
    go :: DirectionalStimulusVerb -> DirectionalStimulusNounPhrase -> GameComputation Identity ()
    go _  _ = pure ()
