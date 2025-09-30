# Sasha: Effect-Driven Text Adventure Engine

## Architecture Overview

Sasha is a Haskell text adventure engine that uses an effect-driven architecture.

## Core Design Principles

### 1. Effect-Driven Architecture
- **Actions produce effects** - Every player action generates ActionEffectKeys
- **Effects are owned by entities** - Player, Location, Object, or System owns specific effects
- **No cross-scope processing** - Effects are always processed by their owning entity

### 2. Management Hierarchy
```
Player Level    → Owns capability-granting effects
Location Level  → Owns environmental effects and implicit actions  
Object Level    → Owns object-specific behaviors
System Level    → Owns perception and cross-cutting effects
```

### 3. Composition Strategy
**Effects first, then actions** - Effects are declared and linked to entities during world building, then actions are resolved based on available capabilities.

### 4. Execution Model
Everything builds to a single computation in TopLevel:
```
Input → Parse → Evaluate → Resolve Actions → Process Effects → Display → Loop
```

## Project Structure

```
/backend/src/
├── ActionDiscovery/      # Transforms a Sentence into possible actions
├── ConstraintRefinement/ # This is the constraint solver that is mis-named
├── EDSL/                 # Embedded DSL for declarative world building
├── Evaluators/           # Command evaluation and routing
├── GameState/            # State management and effect processing
├── Grammar/              # Where the Lexemes get meaning
├── Model/                # What it says on the tin
├── Sasha/                # Effect algebra and type mappings
└── TopLevel.hs           # Main game loop orchestration
```

## Key Components

### Action System
- **ActionDiscovery**: Determines what actions are available given current context
- **ConstraintRefinement**: Validates action feasibility and constructs computation for topLevel.
- **Type-safe action functions**: GADTs ensure compile-time safety

### Effect System
- **ActionEffectKeys**: Links actions to their consequences
- **Effect ownership**: Strict boundaries prevent cross-scope processing
- **System effects**: Handles perception updates and global state sync

### Effect Algebra (Type Families)
The effect algebra uses a type family to map verbs to their action function types,
in service of a simpler DSL interface.
```haskell
type family ActionFunctionType (verb :: Type) :: Type where
  ActionFunctionType ImplicitStimulusVerb = ImplicitStimulusActionF
  ActionFunctionType DirectionalStimulusVerb = DirectionalStimulusActionF
  -- ...

### World Building (EDSL)
```haskell
sashaWorld = do
  locationGID <- declareLocationGID (SimpleNounPhrase locationDesc)
  objectGID <- declareObjectGID (SimpleNounPhrase objectDesc)
  buildEffect actionKey entityKey effectFunction
```

### Parser System
- **Domain-constrained command parsing** 
- **Grammar.Parser**: Tokenization and command structure parsing
- **Model.Parser**: Command structures and sentence types

## Data Flow

1. **Input Processing**: User command → tokenization → parsing
2. **Action Resolution**: Parse tree → action discovery → constraint resolution
3. **Effect Processing**: Valid action → effect lookup → entity-owned processing
4. **State Update**: Effect computation  → game state modification
5. **Output Generation**: Updated state → narrative generation → display

## Type Safety Features

- **GADTs** for type-safe action functions
- **Type families** in effect algebra for verb-to-action mapping
- **Phantom types** (GID system) for entity identification
- **Monad transformers** for layered computation effects

## Extension Points

### Adding New Actions
1. Define action function type in Model.Core
2. Add mapping in ActionFunctionType type family
3. Implement discovery logic in ActionDiscovery
4. Add constraint refinement in ConstraintRefinement
5. Link effects through EDSL

### Adding New Effects
1. Create effect function in appropriate entity module
2. Register with ActionEffectKey
3. Link to actions during world building using makeEffect

## Key Abstractions

- **GameState**: Central state container with entity maps
- **GameComputation**: Primary computation monad (StateT GameState)
- **GameT IO**: IO-lifted game monad for interactive execution
- **SashaLambdaDSL**: World-building monad

## Error Handling

- Left-biased Either types for validation
- Comprehensive error types per failure mode
- Monadic error propagation through computation chains

## Architectural Properties

1. **Modularity**: Clean separation between parsing, resolution, validation, and execution
2. **Type Safety**: Compile-time guarantees prevent runtime errors
3. **Declarative**: Composable EDSL for world construction
4. **Extensibility**: New content additions don't require core system changes
5. **Effect Isolation**: Clear ownership boundaries prevent unexpected interactions
