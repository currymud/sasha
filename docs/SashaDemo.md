# SashaDemo: A Reference Implementation of Compositional Effect Architecture

## What SashaDemo Demonstrates

SashaDemo demonstrates a **compositional effect architecture** built on category theory principles - a system where game behaviors are constructed through type-safe composition of effects and actions, with mathematical guarantees about behavior composition and effect ownership.

## The Core Engineering System

### Type-Safe Compositional Architecture
```haskell
-- Type families ensure verb-to-action correspondence  
type family ActionFunctionType (verb :: *) :: *
ActionFunctionType AcquisitionVerb = AcquisitionActionF

-- Type classes provide unified interfaces
class MakeBehavior verb where
  makeBehavior :: verb -> GID (ActionFunctionType verb) -> ActionManagement

-- Entities get behaviors through unified interface
withBehavior (makeBehavior get getRobeDeniedGID) robeObject
```

**Key insight**: Mathematical composition laws ensure that behaviors and effects compose predictably, while type families guarantee correctness at compile time.

## Compositional Effect Architecture

### 1. Effect Composition Algebra
**Domain-relevant operators** for expressing temporal relationships:
```haskell
-- Effects happening simultaneously
effect key target1 effect1 `alongside`
effect key target2 effect2 `alongside`
effect key target3 effect3 `andThen`

-- Effects happening in sequence  
effect key target4 effect4 `andThen`
effect key target5 effect5
```

**Categorical foundation** ensures composition laws:
```haskell
-- SashaLambdaDSL forms a category with proper composition
buildEffects :: EffectChain -> SashaLambdaDSL ()
-- Composes effect morphisms into single computation
```

### 2. Unified Type-Safe Interfaces
**HasBehavior** provides entity-agnostic behavior attachment:
```haskell
class HasBehavior a where
  withBehavior :: ActionManagement -> a -> SashaLambdaDSL a

-- Works uniformly across all entity types
player   & withBehavior (makeBehavior get getGID)
object   & withBehavior (makeBehavior look lookGID)  
location & withBehavior (makeBehavior isaLook implicitGID)
```

**HasEffect** provides entity-agnostic effect linking:
```haskell
class HasEffect a where
  linkEffect :: ActionEffectKey -> a -> Effect -> SashaLambdaDSL ()

-- Same interface for all target types
linkEffect triggerKey playerTarget effect
linkEffect triggerKey objectTarget effect
```

### 3. Type Families for Compile-Time Safety
**ActionFunctionType** maps verbs to their action function types:
```haskell
type family ActionFunctionType (verb :: *) :: * where
  ActionFunctionType ImplicitStimulusVerb = ImplicitStimulusActionF
  ActionFunctionType DirectionalStimulusVerb = DirectionalStimulusActionF  
  ActionFunctionType AcquisitionVerb = AcquisitionActionF
```

**MakeBehavior** and **MakeEffect** use type families for safety:
```haskell
class MakeBehavior verb where
  makeBehavior :: verb -> GID (ActionFunctionType verb) -> ActionManagement

class MakeEffect verb where  
  makeEffect :: verb -> GID (ActionFunctionType verb) -> SashaLambdaDSL Effect
```

Type families prevent mismatched verb-action pairings at compile time.

### 4. Ownership System → Effect System → Dynamic Dispatch
**Ownership boundaries** control which effects can modify which entities:
```haskell
linkEffectToPlayer :: TargetEffectKey -> PlayerKey -> Effect → only affects Player
linkEffectToObject :: TargetEffectKey → GID Object → Effect → only affects Object
```

**Effect system** processes these ownership-constrained effects:
```haskell
processEffect (PlayerKey pid) effect → modifies Player's dispatch table only
```

**Dynamic dispatch** uses the modified tables respecting ownership boundaries:
```haskell
-- Player effects only change Player's action mappings
-- Object effects only change Object's action mappings
```

### 5. WorldDSL → All Systems Integration
**WorldDSL** orchestrates the setup of all systems:
```haskell
sashaBedroomDemo = do
  -- Creates entities for dispatch system
  robeGID ← declareObjectGID
  
  -- Creates implementations for dispatch tables  
  getRobeDeniedGID ← declareAcquisitionActionGID getRobeDeniedF
  
  -- Links effects respecting ownership boundaries
  linkEffectToObject openEyesEffectKey robeGID robeOpenEyesEffect
  
  -- Registers spatial relationships for constraint processing
  registerSpatial chairGID (Supports robeGID)
```

The DSL ensures all systems are configured consistently and safely.

## System Integration Flow

### Runtime Execution Pipeline
1. **Grammar** parses input → generates ActionManagement key
2. **Dynamic Dispatch** looks up current implementation for key
3. **Constraint Processing** validates and coordinates multi-entity operation
4. **Effect System** may modify dispatch tables based on action results
5. **Ownership System** ensures effects only modify authorized tables

### Effect Processing Pipeline  
1. **Trigger Action** executes (e.g., "open eyes")
2. **Effect System** finds registered effects for that trigger
3. **Ownership System** validates effect can modify target entity
4. **Dynamic Dispatch** tables get updated with new key→implementation mappings
5. **Future Actions** use modified dispatch tables

## Compositional Benefits

### Mathematical Guarantees
The categorical foundation provides composition laws that ensure:
- **Associativity**: `(a `alongside` b) `alongside` c = a `alongside` (b `alongside` c)`  
- **Identity**: Effects compose with identity morphisms correctly
- **Type Safety**: Impossible to create invalid verb-action combinations

### Domain-Expressive Code
Effect relationships read like natural language:
```haskell
-- Opening eyes triggers look effects alongside each other,
-- and then access effects happen one after another
buildEffects $
  lookEffect1 `alongside` lookEffect2 `alongside` lookEffect3 `andThen`
  accessEffect1 `andThen` accessEffect2 `andThen` accessEffect3
```

### Unified Entity Interface
The same patterns work across all entity types:
```haskell
-- Behaviors attach uniformly
player   & withBehavior (makeBehavior verb gid)
object   & withBehavior (makeBehavior verb gid)  
location & withBehavior (makeBehavior verb gid)

-- Effects link uniformly
linkEffect key player effect
linkEffect key object effect
linkEffect key location effect
```

### Compile-Time Correctness
Type families catch errors at compile time rather than runtime:
- Verb types must match their corresponding action function types
- Effect ownership is enforced through the type system
- Composition preserves type safety throughout the chain

## The Reference Value

SashaDemo demonstrates how category theory principles can create practical,
expressive APIs for complex interactive systems.
The implementation serves as a reference for building compositional effect systems
with strong mathematical foundations and intuitive developer experiences.
