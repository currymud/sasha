# Sasha Architecture: Compositional Effect System with Categorical Foundations

This document describes Sasha's core architectural systems built on category theory principles:
 - Type-safe compositional interfaces for behaviors and effects
 - Mathematical composition laws ensuring predictable system behavior  
 - Domain-expressive operators for temporal effect relationships
 - Unified entity interfaces that work across all game object types

## Constrained Interactive Fiction Grammar System

Sasha implements a constrained interactive fiction grammar system that transforms
domain-specific text adventure commands into type-safe action structures,
serving as the entry point to both the dynamic dispatch system and constraint solver.

### Lexical Analysis and Parsing Pipeline

```haskell
-- From TopLevel.hs:64-74
trySentence :: Text -> Either Text Sentence
trySentence input = case lexify tokens input of
  Left err      -> Left ("Lexeme fubar " <> err)
  Right lexemes -> case trySentence' lexemes of
    Left err       -> Left ("Parser fubar " <> err)  
    Right sentence -> Right sentence
```

The parsing pipeline transforms text adventure commands through multiple stages:

1. **Lexical Analysis**: `lexify tokens input` converts text to categorized lexemes
2. **Syntactic Parsing**: `parseTokens lexemes` builds typed sentence structures
3. **Action Key Generation**: Sentences map to specific ActionManagement keys for dispatch

### Compositional Grammar Architecture

The grammar system leverages [case grammar](https://en.wikipedia.org/wiki/Case_grammar),
to build compositional verb phrases that encode intentions:

```haskell
-- From Model.Parser.Composites.Verbs
data Imperative
  = Administrative AdministrativeVerb --depricated
  | ContainerAccessVerbPhrase' ContainerAccessVerbPhrase  
  | StimulusVerbPhrase StimulusVerbPhrase
  | ConsumptionVerbPhrase' ConsumptionVerbPhrase
  | AcquisitionVerbPhrase' AcquisitionVerbPhrase
  | PosturalVerbPhrase PosturalVerbPhrase
```
### Type-Safe Verb-to-Action Mapping

Type families ensure compile-time correspondence between verbs and action functions:
```haskell
-- Type family defines the mapping
type family ActionFunctionType (verb :: Type) :: Type where
  ActionFunctionType ImplicitStimulusVerb = ImplicitStimulusActionF
  ActionFunctionType DirectionalStimulusVerb = DirectionalStimulusActionF  
  ActionFunctionType AcquisitionVerb = AcquisitionActionF
  ActionFunctionType SomaticAccessVerb = SomaticAccessActionF
  ActionFunctionType SimpleAccessVerb = ContainerAccessActionF

-- Type classes use the family for safety
class MakeBehavior verb where
  makeBehavior :: verb -> GID (ActionFunctionType verb) -> ActionManagement
```

Type families prevent mismatched verb-action combinations at compile time, ensuring system integrity.

## Unified Entity Interfaces  

Sasha provides type classes that work uniformly across all entity types, eliminating the need for entity-specific dispatch logic.

### HasBehavior: Universal Behavior Attachment
```haskell
class HasBehavior a where
  withBehavior :: ActionManagement -> a -> SashaLambdaDSL a

-- Works identically for all entity types  
instance HasBehavior Location where
  withBehavior = flip withLocationBehavior

instance HasBehavior Object where
  withBehavior = flip withObjectBehavior

instance HasBehavior Player where
  withBehavior = flip withPlayerBehavior
```

### HasEffect: Universal Effect Linking
```haskell
class HasEffect a where
  linkEffect :: ActionEffectKey -> a -> Effect -> SashaLambdaDSL ()

-- Same interface for all target types
instance HasEffect (GID Location) where
  linkEffect = linkEffectToLocation

instance HasEffect (GID Object) where
  linkEffect = linkEffectToObject

instance HasEffect PlayerKey where
  linkEffect = linkEffectToPlayer
```

### Unified Usage Pattern

The same code patterns work across all entity types:
```haskell
-- Attach behaviors uniformly
player   & withBehavior (makeBehavior verb gid)
object   & withBehavior (makeBehavior verb gid)  
location & withBehavior (makeBehavior verb gid)

-- Link effects uniformly
linkEffect triggerKey playerTarget effect
linkEffect triggerKey objectTarget effect
linkEffect triggerKey locationTarget effect
```

## Compositional Effect Architecture

Sasha provides a compositional algebra for expressing temporal relationships between effects with domain-relevant operators.

### Effect Composition Algebra

Effects are built using type-safe composition with expressive operators:
```haskell
-- Composable effect chain
data EffectChain where
  Single :: (HasEffect a) => ActionEffectKey -> a -> Effect -> EffectChain
  Sequential :: EffectChain -> EffectChain -> EffectChain  
  Parallel :: EffectChain -> EffectChain -> EffectChain

-- Domain-relevant composition operators
infixr 1 `andThen`   -- Sequential: one effect and then another
infixr 2 `alongside` -- Parallel: effects happening simultaneously

andThen :: EffectChain -> EffectChain -> EffectChain
alongside :: EffectChain -> EffectChain -> EffectChain
```

### Expressive Effect Composition

Effects compose naturally using domain language:
```haskell
-- Opening eyes triggers parallel look effects, then sequential access effects
buildEffects $
  effect triggerKey target1 lookEffect1 `alongside`
  effect triggerKey target2 lookEffect2 `alongside`
  effect triggerKey target3 lookEffect3 `alongside`
  effect triggerKey target4 lookEffect4 `andThen`
  
  effect triggerKey target5 accessEffect1 `andThen`
  effect triggerKey target6 accessEffect2 `andThen`
  effect triggerKey target7 accessEffect3
```

### Categorical Foundation

The effect system is built on category theory principles:
- **Objects**: Game states or state transformations
- **Morphisms**: Effect operations that transform game states  
- **Composition**: Monadic composition ensures proper sequencing
- **Identity**: Pure operations that leave state unchanged

```haskell
-- SashaLambdaDSL forms a category
buildEffects :: EffectChain -> SashaLambdaDSL ()
-- Builds morphisms in the SashaLambdaDSL category
```

## The Constraint Solver System

Sasha implements a two-phase constraint solver that operates on the computations
resolved through dynamic dispatch:

### Phase 1: ActionDiscovery - Feasibility Analysis

The ActionDiscovery system determines whether a requested action is feasible:
```
-- From ActionDiscovery modules
manageAcquisitionProcess    -- Analyzes acquisition feasibility
manageContainerAccessProcess -- Analyzes container access feasibility
manageSomaticAccessProcess  -- Analyzes feasibility of body-related actions. e.g "open eyes"
```
### Phase 2: ConstraintRefinement - Multi-Entity Coordination  

The ConstraintRefinement system coordinates execution across multiple entities:

```haskell
-- From ConstraintRefinement modules  
ConstraintRefinement.Actions.Player.Get    -- Player coordination logic
ConstraintRefinement.Actions.Objects.Get   -- Object participation logic
ConstraintRefinement.Actions.Locations     -- Location (not involved in "get" commands)
but has similar purpose
```
## System Integration: Grammar → Dispatch → Effects → Constraint Solver

### Complete Flow Example: "get robe"

1. **Grammar Processing**: `"get robe"` → `AVManagementKey get someGID`
2. **Dynamic Dispatch**: Key lookup in acquisitionActionMap 
3. **Effect Resolution**: Effects may have swapped which GID the key contains
4. **Constraint Processing**: The resolved computation builds through
ActionDiscovery → ConstraintRefinement, then executes TopLevel

### Effect-Driven Behavioral Change

The same input produces different behavior through effect-driven key swapping:
```haskell
-- Before "open eyes":
"get robe" → AVManagementKey get getRobeDeniedGID → acquisitionActionMap lookup → getRobeDeniedF → "You feel dizzy..."

-- After "open eyes" fires effects:
"get robe" → AVManagementKey get getObjectGID → acquisitionActionMap lookup → getObjectF → successful acquisition
```
The GID in the ActionManagement key changes, pointing to a different entry in the action map.

## Engineering Benefits

This compositional architecture provides:

### Mathematical Guarantees
- **Composition Laws**: Categorical foundation ensures associativity and identity laws
- **Type Safety**: Type families prevent invalid verb-action combinations at compile time
- **Predictable Behavior**: Mathematical laws ensure effects compose as expected

### Developer Experience  
- **Domain Expression**: `alongside` and `andThen` read like natural language
- **Unified Interfaces**: Same patterns work across all entity types
- **Compile-Time Errors**: Mismatched types caught early rather than at runtime

### Architectural Benefits
- **Compositional Design**: Effects build complex behaviors from simple parts
- **Separation of Concerns**: Type classes separate interface from implementation
- **Extensibility**: New entity types automatically work with existing interfaces

### Example: Compositional Effect Structure
```haskell
-- Effects compose to express temporal relationships
buildEffects $
  -- Parallel composition: effects that happen simultaneously
  eyeOpenLookBedroom `alongside` eyeOpenLookFloor `alongside` 
  eyeOpenLookChair   `alongside` eyeOpenLookRobe  `andThen`
  
  -- Sequential composition: effects that happen in order  
  enablePocketAccess `andThen` enableRobeGet `andThen`
  addRobeToInventory `andThen` completeAcquisition
```

The result is a system where complex interactive behaviors are built through
mathematical composition of simple effects, providing both correctness guarantees and expressive power.
