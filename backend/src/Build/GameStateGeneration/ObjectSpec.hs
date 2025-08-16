{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Build.GameStateGeneration.ObjectSpec where

import qualified Data.Map.Strict
import qualified Data.Set
import           Data.Text                     (Text)
import           Model.GameState               (ActionManagement,
                                                ActionManagementFunctions (ActionManagementFunctions),
                                                Location (Location, _locationActionManagement, _objectSemanticMap, _title),
                                                Object (Object, _description, _descriptives, _objectActionManagement, _shortName),
                                                Player (Player, _location, _playerActions),
                                                SpatialRelationshipMap (SpatialRelationshipMap),
                                                World (World, _locationMap, _objectMap, _perceptionMap, _spatialRelationshipMap))
import           Model.GID                     (GID (GID))
import           Model.Mappings                (GIDToDataMap (GIDToDataMap))
import           Model.Parser.Atomics.Nouns    (DirectionalStimulus)
import           Model.Parser.Composites.Nouns (DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                NounPhrase)
import           Model.Parser.GCase            (NounKey)


defaultObject :: Object
defaultObject = Object
  { _shortName = ""
  , _description = ""
  , _descriptives = Data.Set.empty
  , _objectActionManagement = ActionManagementFunctions Data.Set.empty
  }

defaultLocation :: Location
defaultLocation = Location
  { _title = ""
  , _objectSemanticMap = Data.Map.Strict.empty
  , _locationActionManagement = ActionManagementFunctions Data.Set.empty
  }

defaultWorld :: World
defaultWorld = World
  { _objectMap = GIDToDataMap Data.Map.Strict.empty
  , _locationMap = GIDToDataMap Data.Map.Strict.empty
  , _perceptionMap = mempty
  , _spatialRelationshipMap = SpatialRelationshipMap Data.Map.Strict.empty
  }

defaultPlayer :: Player
defaultPlayer = Player
  { _location = GID 0  -- Will be overridden
  , _playerActions = ActionManagementFunctions Data.Set.empty
  }

-- | Builder functions for Object fields
withShortName :: Text -> Object -> Object
withShortName name obj = obj { _shortName = name }

withDescription :: Text -> Object -> Object
withDescription desc obj = obj { _description = desc }

withDescriptives :: [NounPhrase DirectionalStimulus] -> Object -> Object
withDescriptives descriptives obj = obj
  { _descriptives = Data.Set.fromList $ map DirectionalStimulusNounPhrase descriptives }

withBehaviors :: [ActionManagement] -> Object -> Object
withBehaviors behaviors obj = obj
  { _objectActionManagement = ActionManagementFunctions (Data.Set.fromList behaviors) }

-- | Builder functions for Location fields
withTitle :: Text -> Location -> Location
withTitle title loc = loc { _title = title }

withObjects :: [(GID Object, NounKey)] -> Location -> Location
withObjects objectGIDsWithKeys loc =
  let objectSemanticMap = Data.Map.Strict.fromListWith Data.Set.union
        [(nounKey, Data.Set.singleton gid) | (gid, nounKey) <- objectGIDsWithKeys]
  in loc { _objectSemanticMap = objectSemanticMap }

withLocationBehaviors :: [ActionManagement] -> Location -> Location
withLocationBehaviors behaviors loc = loc
  { _locationActionManagement = ActionManagementFunctions (Data.Set.fromList behaviors) }

-- | Builder functions for Player fields
withPlayerLocation :: GID Location -> Player -> Player
withPlayerLocation locGID player = player { _location = locGID }

withPlayerBehaviors :: [ActionManagement] -> Player -> Player
withPlayerBehaviors behaviors player = player
  { _playerActions = ActionManagementFunctions (Data.Set.fromList behaviors) }
