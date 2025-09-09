# Sasha Architecture: Constraint Solver, Dynamic Dispatch, and Effects Management

This document describes Sasha's core architectural systems:
 - The constraint solver that manages action feasibility.
 - The dynamic dispatch system that routes actions to appropriate handlers.
 - The effects management system that dynamically swaps action keys,
   and the constrained grammar system that converts domain-specific commands to typed action structures.

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

The grammar system leverages [case grammar](https://en.wikipedia.org/wiki/Case_grammar), to build compositional verb phrases that
encode text adventure action intentions:

```haskell
-- From Model.Parser.Composites.Verbs
data Imperative
  = Administrative AdministrativeVerb
  | ContainerAccessVerbPhrase' ContainerAccessVerbPhrase  
  | StimulusVerbPhrase StimulusVerbPhrase
  | ConsumptionVerbPhrase' ConsumptionVerbPhrase
  | AcquisitionVerbPhrase' AcquisitionVerbPhrase
  | PosturalVerbPhrase PosturalVerbPhrase
```

### Grammar-to-Key Mapping

The grammar system generates ActionManagement keys that serve as lookup keys into action maps:

```haskell
-- From Model.Core.hs:481-494
data ActionManagement
  = DSAManagementKey DirectionalStimulusVerb (GID DirectionalStimulusActionF)
  | AVManagementKey AcquisitionVerb (GID AcquisitionActionF)           -- "get robe" 
  | AAManagementKey AcquisitionVerbPhrase (GID AcquisitionActionF)
  | SAConManagementKey SimpleAccessVerb (GID ContainerAccessActionF)
  | SSAManagementKey SomaticAccessVerb (GID SomaticAccessActionF)      -- "open eyes"
```

Each verb phrase type maps to specific management keys which then resolve to action computations through the dispatch system.

## Dynamic Dispatch System

Sasha uses action maps as lookup tables that can fail if keys are not found. The grammar generates keys that may or may not successfully resolve to computations.

### Action Map Architecture

```haskell
data ActionMaps = ActionMaps
  { _implicitStimulusActionMap           :: Map ImplicitStimulusActionF (GameComputation Identity CoordinationResult)
  , _directionalStimulusActionMap        :: Map DirectionalStimulusActionF (GameComputation Identity CoordinationResult)  
  , _acquisitionActionMap                :: Map AcquisitionActionF (GameComputation Identity CoordinationResult)
  , _containerAccessActionMap            :: Map ContainerAccessActionF (GameComputation Identity CoordinationResult)
  , _somaticStimulusActionMap           :: Map SomaticStimulusActionF (GameComputation Identity CoordinationResult)
  }
```

### Dispatch Process with Failure Handling

1. **Grammar Parsing**: `"get robe"` → `AcquisitionVerbPhrase' getRobeAVP`
2. **Key Generation**: Verb phrase → ActionManagement key for entity
3. **Map Lookup**: Check if key exists in acquisitionActionMap → may fail. But that would be a DSL programmer error.
4. **Execution**: If found, the bound computation executes; if not, action fails, then the DSL programmer needs to fix it.

There should always be some sort of actionF function associated with an action, even if it's a "you can't do that" action.
## Effects Management System: Dynamic Key Swapping

The effects system dynamically swaps which ActionManagement keys entities use to
look up actions in the static action maps.

### Effect Processing Model

Effects work by modifying the ActionManagement keys in entities' ActionManagementFunctions:

```haskell
-- From GameState/ActionManagement.hs:144-149
processEffect (LocationKey lid) (ActionManagementEffect (AddAcquisitionVerb verb newActionGID) _) = do
  modifyLocationActionManagementM lid $ \actionMgmt ->
    let ActionManagementFunctions actionSet = actionMgmt
        filteredActions = Data.Set.filter (\case AVManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (AVManagementKey verb newActionGID) filteredActions
    in ActionManagementFunctions updatedActions
```

### Dynamic Key Swapping Example: "Open Eyes" → "Get Robe"

```haskell
-- Initial state: Object's ActionManagementFunctions contains:
-- AVManagementKey get getRobeDeniedGID

-- After "open eyes" fires effects:
-- 1. Filter out old key: AVManagementKey get getRobeDeniedGID
-- 2. Insert new key: AVManagementKey get getObjectGID  

-- Same grammar parse "get robe" → same verb lookup → different GID → different action
```

### Effect Ownership and Scoping

Effects are owned by specific entities and modify ActionManagementFunctions within their ownership scope:

```haskell
-- From SashaDemo.hs:220-231
linkEffectToPlayer (SomaticAccessActionKey openEyesGID) (PlayerKeyObject robeGID) 
  robeOpenEyesLookChangesGetRobeForPlayer

linkEffectToObject (SomaticAccessActionKey openEyesGID) robeGID 
  robeOpenEyesLookChangesGetRobeForRobe
```

When "open eyes" fires, these effects modify which ActionManagement keys the player and robe use, changing what actions they can successfully perform.

## The Constraint Solver System

Sasha implements a two-phase constraint solver that operates on the computations resolved through dynamic dispatch:

### Phase 1: ActionDiscovery - Feasibility Analysis

The ActionDiscovery system determines whether a requested action is feasible:

```haskell
-- From ActionDiscovery modules
manageAcquisitionProcess    -- Analyzes acquisition feasibility
manageContainerAccessProcess -- Analyzes container access feasibility
manageSomaticAccessProcess  -- Analyzes physical action feasibility
```

### Phase 2: ConstraintRefinement - Multi-Entity Coordination  

The ConstraintRefinement system coordinates execution across multiple entities:

```haskell
-- From ConstraintRefinement modules  
ConstraintRefinement.Actions.Player.Get    -- Player coordination logic
ConstraintRefinement.Actions.Objects.Get   -- Object participation logic
ConstraintRefinement.Actions.Locations     -- Location state updates
```

## System Integration: Grammar → Dispatch → Effects → Constraint Solver

### Complete Flow Example: "get robe"

1. **Grammar Processing**: `"get robe"` → `AVManagementKey get someGID`
2. **Dynamic Dispatch**: Key lookup in acquisitionActionMap 
3. **Effect Resolution**: Effects may have swapped which GID the key contains
4. **Constraint Processing**: The resolved computation executes through ActionDiscovery → ConstraintRefinement

### Effect-Driven Behavioral Change

The same input produces different behavior through effect-driven key swapping:

```haskell
-- Before "open eyes":
"get robe" → AVManagementKey get getRobeDeniedGID → acquisitionActionMap lookup → getRobeDeniedF → "You feel dizzy..."

-- After "open eyes" fires effects:
"get robe" → AVManagementKey get getObjectGID → acquisitionActionMap lookup → getObjectF → successful acquisition
```

The grammar parsing remains identical - only the GID in the ActionManagement key changes, pointing to a different entry in the action map.

## Engineering Benefits

This architecture provides:

- **Predictable Dispatch**: Grammar generates consistent keys; map lookups may succeed or fail
- **Dynamic Behavioral Modification**: Effects swap keys without changing action map structure
- **Type-Safe Action Resolution**: Grammar types ensure valid key generation for text adventure commands
- **Compositional Effects**: Multiple effects can modify the same entity's ActionManagement keys safely
- **Separation of Concerns**: Grammar, dispatch, effects, and constraint solving are cleanly separated

The result is a system where the same text adventure command can produce different behavior
through effect-driven key swapping, while maintaining type safety and predictable dispatch semantics within the constrained interactive fiction domain.
