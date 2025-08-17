{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
module Build.GameStateGeneration.EDSL.GameStateBuilder where

import           Control.Monad                (unless, when)
import           Control.Monad.Except         (ExceptT, MonadError, runExceptT,
                                               throwError)
import           Control.Monad.State.Strict   (MonadState, State, get, modify,
                                               runState)
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Model.GameState              (GameState (..), Location (..),
                                               Object (..), Player (..),
                                               SpatialRelationship (..),
                                               SpatialRelationshipMap (..),
                                               World (..))
import           Model.GameState.GameStateDSL (WorldDSL (..))
import           Model.GID                    (GID)
import           Model.Mappings               (GIDToDataMap (..))

-- | World builder error types
data WorldBuilderError
 = NoPlayerBuilt String
 | PlayerAlreadyBuilt String
 | InvalidObjectGID (GID Object) String
 | InvalidLocationGID (GID Location) String
 | DuplicateObjectGID (GID Object) String
 | DuplicateLocationGID (GID Location) String
 | SpatialRelationshipError (GID Object) String
 | EmptyGameState String
 | InvalidPlayerLocation (GID Location) String
 deriving (Show, Eq)

-- | Error display
displayError :: WorldBuilderError -> String
displayError = \case
 NoPlayerBuilt ctx ->
   "No player built: " <> ctx
 PlayerAlreadyBuilt ctx ->
   "Player already exists: " <> ctx
 InvalidObjectGID gid ctx ->
   "Object GID " <> show gid <> " not found: " <> ctx
 InvalidLocationGID gid ctx ->
   "Location GID " <> show gid <> " not found: " <> ctx
 DuplicateObjectGID gid ctx ->
   "Duplicate object GID " <> show gid <> ": " <> ctx
 DuplicateLocationGID gid ctx ->
   "Duplicate location GID " <> show gid <> ": " <> ctx
 SpatialRelationshipError objGID ctx ->
   "Spatial relationship error for object " <> show objGID <> ": " <> ctx
 EmptyGameState ctx ->
   "Empty game state: " <> ctx
 InvalidPlayerLocation locGID ctx ->
   "Invalid player location " <> show locGID <> ": " <> ctx

-- | Simplified WorldBuilder monad without Reader
newtype WorldBuilder a = WorldBuilder
 (ExceptT WorldBuilderError (State GameState) a)
 deriving newtype ( Functor, Applicative, Monad
                  , MonadState GameState
                  , MonadError WorldBuilderError
                  )

-- | Helper to avoid ambiguous field updates
updateWorldSpatialMap :: World -> SpatialRelationshipMap -> World
updateWorldSpatialMap world newSpatialMap = world { _spatialRelationshipMap = newSpatialMap }

-- | Main interpreter with comprehensive error handling
interpretWorldDSL :: WorldDSL a -> WorldBuilder a
interpretWorldDSL = \case
 Pure a -> return a

 Map f ma -> fmap f (interpretWorldDSL ma)

 Apply mf ma -> do
   f <- interpretWorldDSL mf
   a <- interpretWorldDSL ma
   return (f a)

 Sequence ma mb -> interpretWorldDSL ma >> interpretWorldDSL mb

 Bind ma f -> interpretWorldDSL ma >>= interpretWorldDSL . f

 BuildObject objGID baseObj builderFunc -> do
   let obj = builderFunc baseObj
   -- Note: We build but don't register yet - that's done separately
   return obj

 BuildLocation locGID baseLoc builderFunc -> do
   let loc = builderFunc baseLoc
   -- Note: We build but don't register yet - that's done separately
   return loc

 BuildPlayer basePlayer builderFunc -> do
   -- Check if player already exists (you'll need to define what constitutes "no player")
   gs <- get
   let currentPlayer = gs._player
   -- For now, assume we can always build (your GameState design determines this)
   let player = builderFunc basePlayer
   modify $ \gs' -> gs' { _player = player }
   return player

 RegisterObject objGID obj -> do
   gs <- get
   let GIDToDataMap objectMap = gs._world._objectMap

   -- Check for duplicates
   when (Map.member objGID objectMap) $
     throwError (DuplicateObjectGID objGID
       "Object already registered")

   -- Insert the new object
   let updatedObjectMap = Map.insert objGID obj objectMap
       updatedWorld = gs._world { _objectMap = GIDToDataMap updatedObjectMap }
   modify $ \gs' -> gs' { _world = updatedWorld }

 RegisterObjects mappings -> do
   -- Register each object individually, letting error handling catch issues
   mapM_ (\(objGID, obj) -> interpretWorldDSL (RegisterObject objGID obj)) mappings

 RegisterLocation locGID loc -> do
   gs <- get
   let GIDToDataMap locationMap = gs._world._locationMap

   -- Check for duplicates
   when (Map.member locGID locationMap) $
     throwError (DuplicateLocationGID locGID
       "Location already registered")

   -- Insert the new location
   let updatedLocationMap = Map.insert locGID loc locationMap
       updatedWorld = gs._world { _locationMap = GIDToDataMap updatedLocationMap }
   modify $ \gs' -> gs' { _world = updatedWorld }

 RegisterLocations mappings -> do
   -- Register each location individually
   mapM_ (\(locGID, loc) -> interpretWorldDSL (RegisterLocation locGID loc)) mappings

 SetSpatial objGID relationships -> do
   gs <- get
   let GIDToDataMap objectMap = gs._world._objectMap

   -- Validate that the object exists
   unless (Map.member objGID objectMap) $
     throwError (InvalidObjectGID objGID
       "Object not registered")

   -- Validate each relationship
   mapM_ (validateSpatialRelationship objectMap objGID) relationships

   -- Update spatial relationships using wrapper function
   let SpatialRelationshipMap spatialMap = gs._world._spatialRelationshipMap
       updatedSpatialMap = Map.insert objGID (Set.fromList relationships) spatialMap
       updatedWorld = updateWorldSpatialMap gs._world (SpatialRelationshipMap updatedSpatialMap)
   modify $ \gs' -> gs' { _world = updatedWorld }

 GetCurrentObjects -> do
   gs <- get
   let GIDToDataMap objectMap = gs._world._objectMap
   return $ Map.elems objectMap

 GetCurrentLocations -> do
   gs <- get
   let GIDToDataMap locationMap = gs._world._locationMap
   return $ Map.elems locationMap

 GetCurrentPlayer -> do
   gs <- get
   return $ gs._player

 FinalizeGameState -> do
   gs <- get
   validateFinalGameState gs
   return gs

validateSpatialRelationship :: Map (GID Object) Object -> GID Object -> SpatialRelationship -> WorldBuilder ()
validateSpatialRelationship objectMap sourceObjGID rel = case rel of
 ContainedIn containerGID -> do
   unless (Map.member containerGID objectMap) $
     throwError (SpatialRelationshipError sourceObjGID $
       "Container object " <> show containerGID <> " not found")

 SupportedBy supportGID -> do
   unless (Map.member supportGID objectMap) $
     throwError (SpatialRelationshipError sourceObjGID $
       "Supporting object " <> show supportGID <> " not found")

 Contains objSet -> do
   mapM_ checkContainedObject (Set.toList objSet)
   where
     checkContainedObject gid =
       unless (Map.member gid objectMap) $
         throwError (SpatialRelationshipError sourceObjGID $
           "Contained object " <> show gid <> " not found")

 Supports objSet -> do
   mapM_ checkSupportedObject (Set.toList objSet)
   where
     checkSupportedObject gid =
       unless (Map.member gid objectMap) $
         throwError (SpatialRelationshipError sourceObjGID $
           "Supported object " <> show gid <> " not found")

 Inventory -> return () -- Always valid

validateFinalGameState :: GameState -> WorldBuilder ()
validateFinalGameState gs = do
 -- Check that we have some objects and locations
 let GIDToDataMap objectMap = gs._world._objectMap
     GIDToDataMap locationMap = gs._world._locationMap

 when (Map.null objectMap) $
   throwError (EmptyGameState "No objects registered")

 when (Map.null locationMap) $
   throwError (EmptyGameState "No locations registered")

 -- Check player location exists
 let playerLocation = gs._player._location
 unless (Map.member playerLocation locationMap) $
   throwError (InvalidPlayerLocation playerLocation
     "Player location not found in world")

-- | Simplified runner functions without Config
runWorldBuilder :: WorldBuilder a -> GameState -> (Either WorldBuilderError a, GameState)
runWorldBuilder (WorldBuilder computation) = runState (runExceptT computation)

execWorldBuilder :: WorldBuilder a -> GameState -> Either WorldBuilderError GameState
execWorldBuilder computation gameState =
 case fst $ runWorldBuilder computation gameState of
   Left err -> Left err
   Right _  -> Right $ snd $ runWorldBuilder computation gameState

-- | Main entry points with error handling
buildWorldFromDSL :: GameState -> WorldDSL GameState -> Either String GameState
buildWorldFromDSL initialGameState program =
 case execWorldBuilder (interpretWorldDSL program) initialGameState of
   Left err        -> Left (displayError err)
   Right gameState -> Right gameState

-- | Helper with console output
buildWorldWithOutput :: GameState -> WorldDSL GameState -> IO (Maybe GameState)
buildWorldWithOutput initialGameState program =
 case buildWorldFromDSL initialGameState program of
   Left errMsg -> do
     putStrLn $ "Build failed: " <> errMsg
     return Nothing
   Right gameState -> do
     putStrLn "World built successfully"
     return (Just gameState)
