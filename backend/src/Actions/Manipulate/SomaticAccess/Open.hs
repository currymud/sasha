{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module Actions.Manipulate.SomaticAccess.Open (manageSomaticAccessProcess,savActionEnabled ) where

import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader.Class    (asks)
import qualified Data.Map.Strict
import           GameState                     (getPlayerActionsM)
import           Model.GameState               (ActionMaps (_somaticStimulusActionMap),
                                                Config (_actionMaps),
                                                GameComputation,
                                                PlayerActions (_somaticStimulusActions),
                                                SomaticAccessActionF (SomaticAccessActionF))
import           Model.GID                     (GID)
import           Model.Parser.Atomics.Verbs    (SomaticAccessVerb)
import           Model.Parser.Composites.Nouns (SomaticStimulusNounPhrase)

savActionEnabled :: SomaticAccessVerb -> SomaticAccessActionF
savActionEnabled sav = SomaticAccessActionF actionEnabled
  where
    actionEnabled :: SomaticStimulusNounPhrase -> GameComputation Identity ()
    actionEnabled snp = do
      somaticActionMap <- _somaticStimulusActions <$> getPlayerActionsM
      case Data.Map.Strict.lookup sav somaticActionMap of
        Nothing -> error "Programmer Error: No somatic access action found for verb: "
        Just (actionGID :: GID SomaticAccessActionF) -> do
          actionMap <- asks (_somaticStimulusActionMap . _actionMaps)
          case Data.Map.Strict.lookup actionGID actionMap of
            Nothing -> error $ "Programmer Error: No somatic access action found for GID: "
            Just (SomaticAccessActionF actionFunc) -> actionFunc snp

manageSomaticAccessProcess :: SomaticAccessVerb -> SomaticStimulusNounPhrase -> GameComputation Identity ()
manageSomaticAccessProcess sav snp = do
  availableActions <- _somaticStimulusActions <$> getPlayerActionsM
  case Data.Map.Strict.lookup sav availableActions of
    Nothing -> error $ "Programmer Error: No directional stimulus action found for verb: "
    Just (actionGID :: GID SomaticAccessActionF) -> do
      actionMap <- asks (_somaticStimulusActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error $ "Programmer Error: No directional stimulus action found for GID: "
        Just (SomaticAccessActionF actionFunc) -> actionFunc snp
