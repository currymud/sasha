{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module Actions.Manipulate.SomaticAccess.Open (manageSomaticAccessProcess) where

import           Control.Monad.Identity     (Identity)
import           Control.Monad.Reader.Class (asks)
import qualified Data.Map.Strict
import           GameState                  (getPlayerActionsM)
import           Model.GameState            (ActionMaps (_somaticStimulusActionMap),
                                             Config (_actionMaps),
                                             GameComputation, LocationEffects,
                                             ObjectEffectsMap,
                                             PlayerActions (_somaticStimulusActions),
                                             PlayerEffects,
                                             SomaticAccessActionF (SomaticAccessActionF))
import           Model.GID                  (GID)
import           Model.Parser.Atomics.Verbs (SomaticAccessVerb)

manageSomaticAccessProcess :: SomaticAccessVerb
                                -> PlayerEffects
                                -> LocationEffects
                                -> ObjectEffectsMap
                                -> GameComputation Identity ()
manageSomaticAccessProcess sav playerEffects locationEffects objEffectsMap = do
  availableActions <- _somaticStimulusActions <$> getPlayerActionsM
  case Data.Map.Strict.lookup sav availableActions of
    Nothing -> error "Programmer Error: No directional stimulus action found for verb: "
    Just (actionGID :: GID SomaticAccessActionF) -> do
      actionMap <- asks (_somaticStimulusActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error $ "Programmer Error: No directional stimulus action found for GID: "
        Just (SomaticAccessActionF actionFunc) -> actionFunc playerEffects locationEffects objEffectsMap
