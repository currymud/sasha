module ActionDiscovery.Movement.Postural.SitStand  (managePosturalProcess) where

import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader.Class    (asks)
import qualified Data.Map.Strict
import           GameState                     (getLocationM,
                                                getPlayerLocationGID,
                                                getPlayerM)
import           GameState.ActionManagement    (lookupAgentPostural,
                                                lookupLocationPostural)
import           Model.Core                    (ActionEffectKey (AgentPosturalActionKey, LocationPosturalActionKey),
                                                ActionMaps (_agentPosturalActionMap, _locationPosturalActionMap, _posturalActionMap),
                                                AgentPosturalActionF (AgentCanPosturalF, AgentCannotPosturalF),
                                                Config (_actionMaps),
                                                GameComputation,
                                                Location (_locationActionManagement),
                                                LocationPosturalActionF (LocationCanPosturalF, LocationCannotPosturalF),
                                                Player (_playerActions))
import           Model.GID                     (GID)
import           Model.Parser.Composites.Verbs (PosturalVerbPhrase)


-- Simplified version with helper functions
managePosturalProcess :: PosturalVerbPhrase -> GameComputation Identity ()
managePosturalProcess posturalPhrase = do
  -- Get available actions
  playerAvailableActions <- _playerActions <$> getPlayerM
  locationAvailableActions <- _locationActionManagement <$> (getPlayerLocationGID >>= getLocationM)

  -- Get action GIDs
  let maybeAgentGID = lookupAgentPostural posturalPhrase playerAvailableActions
      maybeLocationGID = lookupLocationPostural posturalPhrase locationAvailableActions

  -- Get action maps once
  agentActionMap <- asks (_agentPosturalActionMap . _actionMaps)
  locationActionMap <- asks (_locationPosturalActionMap . _actionMaps)

  -- Execute based on available actions
  case maybeAgentGID of
    Nothing -> error "Programmer Error: No postural action found for phrase"
    Just agentGID -> do
      let agentAction = Data.Map.Strict.lookup agentGID agentActionMap
      executeAgentAction agentGID agentAction

      -- Execute location action if available
      case maybeLocationGID of
        Nothing -> pure ()
        Just locationGID -> do
          let locationAction = Data.Map.Strict.lookup locationGID locationActionMap
          executeLocationAction locationGID locationAction

-- Helper to execute agent postural action
executeAgentAction :: GID AgentPosturalActionF
                        -> Maybe AgentPosturalActionF
                        -> GameComputation Identity ()
executeAgentAction gid Nothing =
  error $ "Programmer Error: No agent postural action found for GID: " ++ show gid
executeAgentAction gid (Just action) =
  let effectKey = AgentPosturalActionKey gid
  in case action of
       AgentCanPosturalF f    -> f effectKey
       AgentCannotPosturalF f -> f effectKey

-- Helper to execute location postural action
executeLocationAction :: GID LocationPosturalActionF
                          -> Maybe LocationPosturalActionF
                          -> GameComputation Identity ()
executeLocationAction gid Nothing =
  error $ "Programmer Error: No location postural action found for GID: " ++ show gid
executeLocationAction gid (Just action) =
  let effectKey = LocationPosturalActionKey gid
  in case action of
       LocationCanPosturalF f    -> f effectKey
       LocationCannotPosturalF f -> f effectKey
