# SashaDemo: A Reference Implementation of Effect-Driven Dynamic Dispatch

## What SashaDemo Demonstrates

SashaDemo demonstrates **effect-driven dynamic dispatch** - a system where runtime behavior changes are managed through effects that modify dispatch table mappings, combined with type-enforced ownership boundaries that prevent unauthorized cross-component modifications.

## The Core Engineering System

### Effect-Driven Dynamic Dispatch
```haskell
-- Dispatch tables map keys to implementations
AVManagementKey get getRobeDeniedGID

-- Effects modify which implementation a key points to
openEyesEffect = swapActionMapping get getObjectGID

-- Same key, different behavior after effect processing
AVManagementKey get getObjectGID
```

**Key insight**: State is represented by which functions are bound to which keys. State changes happen by swapping function bindings, not by updating variables.

## Relationship Between Sasha's Engineering Systems

### 1. Grammar System → Dynamic Dispatch System
**Grammar parsing** generates ActionManagement keys:
```haskell
"get robe" → AcquisitionVerbPhrase → AVManagementKey get someGID
```

**Dynamic dispatch** uses these keys to look up implementations:
```haskell
lookupAcquisitionVerb (AVManagementKey get) entityActionMap → currentImplementation
```

The grammar system provides the keys; dispatch system provides the lookup mechanism.

### 2. Effect System → Dynamic Dispatch System  
**Effect processing** modifies which implementations keys point to:
```haskell
processEffect openEyesEffect → swaps get key from getRobeDeniedGID to getObjectGID
```

**Dynamic dispatch** uses the modified mappings for subsequent lookups:
```haskell
-- Same key, different implementation after effect processing
AVManagementKey get → now points to getObjectGID instead of getRobeDeniedGID
```

The effect system changes the dispatch tables; dispatch system uses the changed tables.

### 3. Constraint Processing → Multi-Entity Coordination
**ActionDiscovery** determines if an action is feasible across entities:
```haskell
-- Can player get robe? Check all participating entities
discoverAcquisition :: Player -> Object -> Location → Feasibility
```

**ConstraintRefinement** coordinates the actual execution:
```haskell
-- Each entity handles its part of "get robe"
getF        -- Player orchestrates
getFromSupportF  -- Chair releases  
getObjectF      -- Robe adds to inventory
```

Constraint processing validates; coordination executes the distributed operation.

### 4. Ownership System → Effect System → Dynamic Dispatch
**Ownership boundaries** control which effects can modify which entities:
```haskell
linkEffectToPlayer :: ActionEffectKey -> PlayerKey -> Effect → only affects Player
linkEffectToObject :: ActionEffectKey → GID Object → Effect → only affects Object
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

## Engineering Benefits of System Integration

### Type-Safe Runtime Behavior Modification
The ownership system ensures effects can't break dispatch table integrity, while the effect system enables safe runtime behavior changes.

### Compositional Architecture
Each system has clear boundaries and interfaces:
- Grammar → produces keys
- Dispatch → consumes keys, produces implementations  
- Effects → modify dispatch mappings
- Constraints → coordinate implementations
- Ownership → controls effect scope

### Distributed Coordination Without Shared State
Multi-entity operations coordinate through the constraint system while each entity maintains its own dispatch table through the ownership system.

## The Reference Value

SashaDemo shows these five engineering systems working together as an integrated architecture. Each system handles its specific concern while composing cleanly with the others through well-defined interfaces and type-level safety guarantees.

The implementation demonstrates how effect-driven dynamic dispatch can serve as a foundation for building systems that need runtime behavior modification with strong safety properties.