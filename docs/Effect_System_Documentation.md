# Sasha Effect System Documentation

## Overview

Sasha implements an effect system for interactive fiction based on four core principles from the project instructions:

1. **The management hierarchy** - each level owns its effects
2. **The composition strategy** - effects first, then actions  
3. **The execution model** - everything builds to one computation for topLevel
4. **The effect ownership** - no cross-scope effect processing

## Effect Ownership Hierarchy

Each entity type owns and manages its own effects through `ActionManagementFunctions`:

```haskell
data Player = Player
  { _location      :: GID Location
  , _inventory     :: Set (GID Object)
  , _playerActions :: ActionManagementFunctions
  }

data Location = Location
  { _title                    :: Text
  , _objectSemanticMap        :: Map NounKey (Set (GID Object))
  , _locationActionManagement :: ActionManagementFunctions
  }

data Object = Object
  { _shortName              :: Text
  , _description            :: Text
  , _descriptives           :: Set DirectionalStimulusNounPhrase
  , _objectActionManagement :: ActionManagementFunctions
  }
```

## Effect Composition Strategy

Effects are declared first using algebraic combinators, then actions are bound to execute them:

```haskell
data EffectChain where
  Single :: (HasEffect a) => EffectActionKey -> a -> Effect -> EffectChain
  Sequential :: EffectChain -> EffectChain -> EffectChain  
  Parallel :: EffectChain -> EffectChain -> EffectChain

-- Composition operators
infixr 1 `andThen`   -- Sequential composition
infixr 2 `alongside` -- Parallel composition

-- Example from SashaDemo.hs
buildEffects $
  effect (SomaticAccessActionKey openEyesGID) bedroomGID openEyesLookChangeEffectPlayer `alongside`
  effect (SomaticAccessActionKey openEyesGID) floorGID openEyesLookChangeEffectFloor `alongside`
  effect (SomaticAccessActionKey openEyesGID) chairGID openeEyesLooKChangeEffectChair `alongside`
  effect (SomaticAccessActionKey openEyesGID) robeGID openEyesLookChangeEffectRobe `andThen`
  effect (SomaticAccessActionKey openEyesGID) (PlayerKeyObject pocketGID) openEyesOpenPocketChangesForPlayer
```

## Execution Model

All game processing flows through a single pipeline in `TopLevel.hs`:

```haskell
topLevel :: GameT IO ()
topLevel = runGame initComp

runGame :: GameComputation Identity () -> GameT IO ()
runGame comp' = do
  transformToIO comp'           -- Execute computation
  liftDisplay displayResult     -- Display results  
  transformToIO clearNarration  -- Reset narration
  attSentence <- trySentence <$> liftIO getInput
  case attSentence of
    Left err       -> runGame $ errorHandler err
    Right sentence -> runGame $ processWithSystemEffects sentence
```

The flow is: Text → Lexer → Parser → Evaluator → Action Discovery → Effect Processing → State Update

## Effect Types

The system defines two main effect types:

```haskell
data Effect
  = ActionManagementEffect ActionManagementOperation ActionGID
  | FieldUpdateEffect FieldUpdateOperation
  deriving stock (Show, Eq, Ord)

data SystemEffect
  = PerceptionSystemEffect (GameComputation Identity ())
```

Effects are stored in registries and processed through the ownership hierarchy.

## Type-Safe Verb-to-Action Mapping

Type families ensure compile-time safety when mapping verbs to action functions:

```haskell
type family ActionFunctionType (verb :: Type) :: Type where
  ActionFunctionType ImplicitStimulusVerb = ImplicitStimulusActionF
  ActionFunctionType DirectionalStimulusVerb = DirectionalStimulusActionF
  ActionFunctionType AcquisitionVerb = AcquisitionActionF
  ActionFunctionType SomaticAccessVerb = SomaticAccessActionF
  ActionFunctionType SimpleAccessVerb = ContainerAccessActionF
  ActionFunctionType AcquisitionVerbPhrase = AcquisitionActionF
  ActionFunctionType ContainerAccessVerbPhrase = ContainerAccessActionF
```

## Action Discovery Pattern

Actions are discovered through the ownership hierarchy. Example from `ActionDiscovery/Percieve/Look.hs`:

```haskell
manageImplicitStimulusProcess :: ImplicitStimulusVerb -> GameComputation Identity ()
manageImplicitStimulusProcess isv = do
  -- 1. Get player's available actions (respects ownership)
  availableActions <- _playerActions <$> getPlayerM
  
  -- 2. Look up action for this verb in player's action set
  case lookupImplicitStimulus isv availableActions of
    Nothing -> error "Programmer Error: No implicit stimulus action found for verb: in player "
    Just actionGID -> do
      -- 3. Retrieve action function from global action map
      actionMap :: ImplicitStimulusActionMap <- asks (_implicitStimulusActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No implicit stimulus action found for GID: "
        Just (ImplicitStimulusActionF actionFunc) -> do
          -- 4. Execute action with appropriate context
          player <- getPlayerM
          let lid = player._location
          loc <- getLocationM lid
          actionFunc player loc
```

## Effect Processing

Effects are processed through `GameState/ActionManagement.hs` while respecting ownership boundaries:

```haskell
-- ✓ Entity processes effects that target it
processEffect (ObjectKey oid) (ActionManagementEffect operation _) = do
  modifyObjectActionManagementM oid $ \actionMgmt ->
    updateActionManagement operation actionMgmt

-- ✓ Effects can update any entity's fields 
processEffect (LocationKey _) (FieldUpdateEffect (ObjectShortName targetOid newShortName)) = do
  modifyObjectM targetOid $ \obj -> obj { _shortName = newShortName }
```

No entity can directly modify another entity's action management - that would violate the ownership principle.

## DSL and Builder

The `SashaLambdaDSL` provides a typed interface for building game worlds:

```haskell
-- Declare entities and get GIDs
bedroomGID <- declareLocationGID bedroomDS
robeGID <- declareObjectGID robeDS

-- Create actions
openEyesGID <- declareSomaticActionGID openEyes

-- Create and link effects
openEyesEffect <- createSomaticAccessEffect saOpen openEyesGID
linkEffect (SomaticAccessActionKey openEyesGID) bedroomGID openEyesEffect

-- Register entities with behaviors
registerLocation bedroomGID $ 
  withTitle "Your Bedroom" $ 
  withBehavior (ISAManagementKey isaLook defaultLookGID) defaultLocation
```

The builder (`EDSL/GameBuilder.hs`) interprets this DSL and constructs the final game state.


## Key Patterns

1. **Effect Declaration Before Action Binding**: Effects are specified compositionally first, then bound to actions
2. **Hierarchical Ownership**: Each entity owns its actions, no cross-entity action management modification
3. **Type-Safe Dispatch**: Verbs map to specific action function types at compile time
4. **Centralized Execution**: All processing flows through the single `topLevel` computation
5. **Monadic Effect Processing**: All effects processed within the `GameComputation` context

The system separates concerns cleanly: effect specification, action implementation, entity ownership, and execution context are all handled independently.
