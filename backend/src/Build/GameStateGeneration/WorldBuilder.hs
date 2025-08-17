module Build.GameStateGeneration.WorldBuilder where

import           Build.GameStateGeneration.LocationSpec.LocationGIDs     (bedroomGID)
import           Build.GameStateGeneration.LocationSpec.Locations        (locationMap)
import           Build.GameStateGeneration.ObjectSpec.Objects            (objectMap)
import           Build.GameStateGeneration.WorldGeneration               (ObjectBehaviorSpec (ObjectBehaviorSpec, _behaviors, _objectGID),
                                                                          PlayerSpec (PlayerSpec, _playerBehaviors, _playerLocationGID),
                                                                          WorldSpec (WorldSpec, _locationSpecs, _objectSpecs, _playerSpec, _spatialRelationships))
-- Import the reorganized effect registry
import           Build.GameStateGeneration.EffectRegistry                (effectRegistry)

import qualified Data.Bifunctor
import qualified Data.Map.Strict
import qualified Data.Set
import           Data.Text                                               (Text)
import           Evaluators.Player.General                               (eval)
import qualified Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb
import qualified Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb
import qualified Grammar.Parser.Partitions.Verbs.SomaticAccessVerbs
import           Model.GameState                                         (ActionManagementFunctions (ActionManagementFunctions),
                                                                          GameState (GameState, _effectRegistry, _evaluation, _narration, _player, _world),
                                                                          Narration (Narration, _actionConsequence, _playerAction),
                                                                          Object,
                                                                          Player (Player, _location, _playerActions),
                                                                          SpatialRelationship,
                                                                          SpatialRelationshipMap (SpatialRelationshipMap),
                                                                          World (World, _locationMap, _objectMap, _perceptionMap, _spatialRelationshipMap))
import           Model.GID                                               (GID)
import           Model.Parser.Atomics.Verbs                              (DirectionalStimulusVerb,
                                                                          ImplicitStimulusVerb,
                                                                          SomaticAccessVerb)

-- =============================================================================
-- WORLD BUILDING FUNCTIONS
-- =============================================================================

-- | Build world from specification (uses DSL-generated maps)
buildWorldFromSpec :: WorldSpec -> World
buildWorldFromSpec spec = World
  { _objectMap = objectMap
  , _locationMap = locationMap
  , _perceptionMap = mempty
  , _spatialRelationshipMap = buildSpatialMap (_spatialRelationships spec)
  }

-- | Build player from specification
buildPlayerFromSpec :: PlayerSpec -> Player
buildPlayerFromSpec spec = Player
  { _location = _playerLocationGID spec
  , _playerActions = ActionManagementFunctions $ Data.Set.fromList (_playerBehaviors spec)
  }

-- | Build complete game state from specifications
buildGameStateFromSpec :: WorldSpec -> PlayerSpec -> GameState
buildGameStateFromSpec w_spec p_spec = GameState
  { _world = buildWorldFromSpec w_spec
  , _player = buildPlayerFromSpec p_spec
  , _narration = initNarration
  , _evaluation = eval
  , _effectRegistry = effectRegistry  -- Use the reorganized effect registry
  }

-- | Initial game narration
initNarration :: Narration
initNarration = Narration
  { _playerAction = [initialAction]
  , _actionConsequence = mempty
  }

initialAction :: Text
initialAction = "It was a rough night. You smoked your mind the night before, on cigarettes and songs that you'd been pickin'."

-- | Build spatial relationship map from specifications
buildSpatialMap :: [(GID Object, [SpatialRelationship])] -> SpatialRelationshipMap
buildSpatialMap spatialSpecs = SpatialRelationshipMap $ Data.Map.Strict.fromList $
  map (Data.Bifunctor.second Data.Set.fromList) spatialSpecs
