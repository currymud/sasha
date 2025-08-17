module Build.GameState where

import           Build.GameStateGeneration.BedroomWorldSpec (bedroomPlayerSpec,
                                                             bedroomWorldSpec)
import           Build.GameStateGeneration.WorldBuilder     (buildGameStateFromSpec)
import           Build.Identifiers.Actions                  (acquisitionActionMap,
                                                             consumptionActionMap,
                                                             directionalStimulusActionMap,
                                                             implicitStimulusActionMap,
                                                             posturalActionMap,
                                                             somaticAccessActionMap)
import           Model.GameState                            (ActionMaps (ActionMaps),
                                                             Config (Config, _actionMaps),
                                                             GameState)

-- This is your complete game state!
gameState :: GameState
gameState = buildGameStateFromSpec bedroomWorldSpec bedroomPlayerSpec

config :: Config
config = Config
  { _actionMaps = actionMaps
  }
  where
    actionMaps :: ActionMaps
    actionMaps = ActionMaps
                   implicitStimulusActionMap
                   directionalStimulusActionMap
                   somaticAccessActionMap
                   acquisitionActionMap
                   consumptionActionMap
                   posturalActionMap

