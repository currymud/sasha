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
import           Model.GameState              (ActionManagement (..),
                                               ActionManagementFunctions (..),
                                               GameState (..), Location (..),
                                               Object (..), Player (..),
                                               SpatialRelationship (..),
                                               SpatialRelationshipMap (..),
                                               World (..))
import           Model.GameState.GameStateDSL (WorldDSL (..))
import           Model.GID                    (GID (..))
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

-- | Simplified WorldBuilder monad
newtype WorldBuilder a = WorldBuilder
 (ExceptT WorldBuilderError (State GameState) a)
 deriving newtype ( Functor, Applicative, Monad
                  , MonadState GameState
                  , MonadError WorldBuilderError
                  )

-- | Main interpreter - just handle the GADT operations directly
interpretWorldDSL :: WorldDSL a -> WorldBuilder a
interpretWorldDSL = \case
 Pure a -> return a
 Map f ma -> fmap f (interpretWorldDSL ma)
 Apply mf ma -> interpretWorldDSL mf <*> interpretWorldDSL ma
 Sequence ma mb -> interpretWorldDSL ma >> interpretWorldDSL mb
 Bind ma f -> interpretWorldDSL ma >>= interpretWorldDSL . f

 -- GID declarations - TODO: need actual GID generation strategy
 DeclareObjectGID _nounPhrase -> return (GID 1) -- Placeholder
 DeclareObjectiveGID _nounPhrase -> return (GID 1) -- Placeholder
 DeclareConsumableGID _nounPhrase -> return (GID 1) -- Placeholder
 DeclareContainerGID _nounPhrase -> return (GID 1) -- Placeholder
 DeclareLocationGID _nounPhrase -> return (GID 1) -- Placeholder

 -- Initial value constructors
 InitialObject obj -> return obj
 InitialLocation loc -> return loc
 InitialPlayer player -> return player

 -- Builder constructors
 BuildObject objGID baseObj builderFunc -> do
   gid <- interpretWorldDSL objGID
   base <- interpretWorldDSL baseObj
   return (builderFunc base)

 BuildLocation locGID baseLoc builderFunc -> do
   gid <- interpretWorldDSL locGID
   base <- interpretWorldDSL baseLoc
   return (builderFunc base)

 BuildPlayer basePlayer builderFunc -> do
   base <- interpretWorldDSL basePlayer
   return (builderFunc base)

 -- Registration
 RegisterObject objGID obj -> do
   gid <- interpretWorldDSL objGID
   object <- interpretWorldDSL obj
   gs <- get
   let GIDToDataMap objectMap = _objectMap (_world gs)
   when (Map.member gid objectMap) $
     throwError (DuplicateObjectGID gid "Object already registered")
   let updatedObjectMap = Map.insert gid object objectMap
       updatedWorld = (_world gs) { _objectMap = GIDToDataMap updatedObjectMap }
   modify $ \gs' -> gs' { _world = updatedWorld }
   return gid

 RegisterLocation locGID loc -> do
   gid <- interpretWorldDSL locGID
   location <- interpretWorldDSL loc
   gs <- get
   let GIDToDataMap locationMap = _locationMap (_world gs)
   when (Map.member gid locationMap) $
     throwError (DuplicateLocationGID gid "Location already registered")
   let updatedLocationMap = Map.insert gid location locationMap
       updatedWorld = (_world gs) { _locationMap = GIDToDataMap updatedLocationMap }
   modify $ \gs' -> gs' { _world = updatedWorld }
   return gid

 -- NEW: Object-based behavior and spatial operations
 WithBehavior obj actionManagement -> do
   management <- interpretWorldDSL (pure actionManagement)
   let ActionManagementFunctions currentBehaviors = _objectActionManagement obj
       updatedBehaviors = Set.insert management currentBehaviors
   return obj { _objectActionManagement = ActionManagementFunctions updatedBehaviors }

 WithSpatial obj spatialRelationships ->
   return obj -- Spatial handled separately for now

 WithPlayerBehavior playerDSL actionManagementDSL -> do
   player <- interpretWorldDSL playerDSL
   management <- interpretWorldDSL actionManagementDSL
   let ActionManagementFunctions currentBehaviors = _playerActions player
       updatedBehaviors = Set.insert management currentBehaviors
   return player { _playerActions = ActionManagementFunctions updatedBehaviors }

 -- ActionManagement construction
 CreateISAManagement verbDSL gidDSL -> do
   verb <- interpretWorldDSL verbDSL
   gid <- interpretWorldDSL gidDSL
   return $ ISAManagementKey verb gid

 CreateDSAManagement verbDSL gidDSL -> do
   verb <- interpretWorldDSL verbDSL
   gid <- interpretWorldDSL gidDSL
   return $ DSAManagementKey verb gid

 CreateSSAManagement verbDSL gidDSL -> do
   verb <- interpretWorldDSL verbDSL
   gid <- interpretWorldDSL gidDSL
   return $ SSAManagementKey verb gid

 CreateAAManagement verbPhraseDSL gidDSL -> do
   verbPhrase <- interpretWorldDSL verbPhraseDSL
   gid <- interpretWorldDSL gidDSL
   return $ AAManagementKey verbPhrase gid

 CreateAVManagement verbDSL gidDSL -> do
   verb <- interpretWorldDSL verbDSL
   gid <- interpretWorldDSL gidDSL
   return $ AVManagementKey verb gid

 CreateCAManagement verbPhraseDSL gidDSL -> do
   verbPhrase <- interpretWorldDSL verbPhraseDSL
   gid <- interpretWorldDSL gidDSL
   return $ CAManagementKey verbPhrase gid

 CreatePPManagement verbDSL gidDSL -> do
   verb <- interpretWorldDSL verbDSL
   gid <- interpretWorldDSL gidDSL
   return $ PPManagementKey verb gid

 CreateNPManagement verbDSL gidDSL -> do
   verb <- interpretWorldDSL verbDSL
   gid <- interpretWorldDSL gidDSL
   return $ NPManagementKey verb gid

 -- Player management
 SetPlayer playerDSL -> do
   player <- interpretWorldDSL playerDSL
   modify $ \gs -> gs { _player = player }

 UpdatePlayer playerDSL builderFunc -> do
   player <- interpretWorldDSL playerDSL
   let updatedPlayer = builderFunc player
   modify $ \gs -> gs { _player = updatedPlayer }
   return updatedPlayer

 -- Complex builder operations
 ModifyObject objGID builderFunc -> do
   gid <- interpretWorldDSL objGID
   gs <- get
   let GIDToDataMap objectMap = _objectMap (_world gs)
   case Map.lookup gid objectMap of
     Nothing -> throwError (InvalidObjectGID gid "Object not found")
     Just obj -> do
       let modifiedObj = builderFunc obj
           updatedObjectMap = Map.insert gid modifiedObj objectMap
           updatedWorld = (_world gs) { _objectMap = GIDToDataMap updatedObjectMap }
       modify $ \gs' -> gs' { _world = updatedWorld }

 ModifyLocation locGID builderFunc -> do
   gid <- interpretWorldDSL locGID
   gs <- get
   let GIDToDataMap locationMap = _locationMap (_world gs)
   case Map.lookup gid locationMap of
     Nothing -> throwError (InvalidLocationGID gid "Location not found")
     Just loc -> do
       let modifiedLoc = builderFunc loc
           updatedLocationMap = Map.insert gid modifiedLoc locationMap
           updatedWorld = (_world gs) { _locationMap = GIDToDataMap updatedLocationMap }
       modify $ \gs' -> gs' { _world = updatedWorld }

 -- Effect processing
 ProcessEffectsIntoRegistry -> return ()

 -- Context queries
 GetCurrentObjects -> do
   gs <- get
   let GIDToDataMap objectMap = _objectMap (_world gs)
   return $ Map.elems objectMap

 GetCurrentLocations -> do
   gs <- get
   let GIDToDataMap locationMap = _locationMap (_world gs)
   return $ Map.elems locationMap

 GetCurrentPlayer -> do
   gs <- get
   return $ _player gs

 -- Final assembly
 FinalizeGameState -> do
   gs <- get
   validateFinalGameState gs
   return gs

 -- Stubs for other constructors - implement as needed
 _ -> error "Constructor not yet implemented"

validateFinalGameState :: GameState -> WorldBuilder ()
validateFinalGameState gs = do
 let GIDToDataMap objectMap = _objectMap (_world gs)
     GIDToDataMap locationMap = _locationMap (_world gs)

 when (Map.null objectMap) $
   throwError (EmptyGameState "No objects registered")

 when (Map.null locationMap) $
   throwError (EmptyGameState "No locations registered")

 let playerLocation = _location (_player gs)
 unless (Map.member playerLocation locationMap) $
   throwError (InvalidPlayerLocation playerLocation
     "Player location not found in world")

-- | Runner functions
runWorldBuilder :: WorldBuilder a -> GameState -> (Either WorldBuilderError a, GameState)
runWorldBuilder (WorldBuilder computation) = runState (runExceptT computation)

execWorldBuilder :: WorldBuilder a -> GameState -> Either WorldBuilderError GameState
execWorldBuilder computation gameState =
 case fst $ runWorldBuilder computation gameState of
   Left err -> Left err
   Right _  -> Right $ snd $ runWorldBuilder computation gameState

-- | Main entry points
buildWorldFromDSL :: GameState -> WorldDSL GameState -> Either String GameState
buildWorldFromDSL initialGameState program =
 case execWorldBuilder (interpretWorldDSL program) initialGameState of
   Left err        -> Left (displayError err)
   Right gameState -> Right gameState

buildWorldWithOutput :: GameState -> WorldDSL GameState -> IO (Maybe GameState)
buildWorldWithOutput initialGameState program =
 case buildWorldFromDSL initialGameState program of
   Left errMsg -> do
     putStrLn $ "Build failed: " <> errMsg
     return Nothing
   Right gameState -> do
     putStrLn "World built successfully"
     return (Just gameState)
