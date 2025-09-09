# Sasha Overview

**Sasha** is a Haskell-based interactive fiction engine that implements sophisticated effect management and multi-entity action coordination through strict type-level ownership boundaries.

## Architecture

Sasha's design centers around four core principles:

- **Management Hierarchy**: Each level (location, object, player) owns its effects and cannot modify other levels' state directly
- **Composition Strategy**: Effects are processed first to modify action mappings, then actions execute with the updated mappings  
- **Execution Model**: All operations build to a single computation that executes in `topLevel`
- **Effect Ownership**: Cross-scope effect processing is prevented through type-level boundaries

## Type-Driven Design

The system uses Haskell's type system to enforce architectural invariants:

```haskell
-- Action responsibilities encoded in types
data AcquisitionActionF
  = CollectedF (GameComputation Identity CoordinationResult)      -- Object handles being collected
  | LosesObjectF (GID Object -> GameComputation Identity CoordinationResult) -- Container handles loss
  | NotGettableF (GameComputation Identity ())                    -- Object refuses collection
```

Effect ownership is enforced through dedicated linking functions:
- `linkEffectToPlayer` - effects that modify player behavior
- `linkEffectToObject` - effects that modify object behavior  
- `linkEffectToLocation` - effects that modify location behavior

## Monadic Structure

The engine is built on a stack of monad transformers:

```haskell
type GameComputation m a = ReaderT Config (ExceptT Text (GameStateT m)) a
type GameT m a = ReaderT Config (ExceptT Text (GameStateT m)) a
```

This provides:
- **Configuration access** through `ReaderT Config`
- **Error handling** through `ExceptT Text`  
- **Game state management** through `GameStateT`
- **IO capabilities** when needed through `transformToIO`

## Action Processing Pipeline

Actions flow through a two-phase pipeline:

1. **ActionDiscovery**: Determines if the player is capable of the requested action
2. **ConstraintRefinement**: Coordinates multi-entity responsibilities for successful actions

For example, "get robe" involves:
- **Player**: Orchestrates the overall process (`getF`)
- **Chair**: Removes spatial relationships (`getFromSupportF`) 
- **Robe**: Adds itself to player inventory (`getObjectF`)

## Effect System

The effect system allows actions to modify what other actions do:

- Effects are created during world building and linked to specific entities
- When trigger actions execute (e.g., "open eyes"), effects modify action mappings
- Subsequent actions use the modified mappings, enabling different behaviors

This creates dynamic gameplay where the same command can produce different results based on previous actions.

## World Building DSL

Games are constructed using a domain-specific language that handles:

- **Entity Declaration**: Creating GIDs for locations, objects, and actions
- **Behavior Assignment**: Linking actions to entities through management keys
- **Spatial Relationships**: Defining containment and support relationships
- **Effect Registration**: Connecting trigger actions to behavioral changes

The DSL ensures consistent world construction while maintaining type safety.

## Key Benefits

- **Type Safety**: Architectural constraints are enforced at compile time
- **Modular Design**: Clear separation between action discovery, constraint refinement, and effect processing  
- **Multi-Entity Coordination**: Complex actions naturally decompose into entity-specific responsibilities
- **Dynamic Behavior**: Effect system enables rich, stateful gameplay mechanics
- **Testability**: End-to-end scenarios validate complete action flows from input to state changes

The result is an interactive fiction engine that combines rigorous architecture with practical gameplay mechanics, ensuring both correctness and extensibility.