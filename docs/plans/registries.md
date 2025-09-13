# Registries - Secondary Structures

## Overview
Registries are secondary structures that organize and manage the relationships between locks, meters, and game elements. They provide the mapping layer between actions and their effects.

## Current Registry Types in Codebase

### EffectRegistry
- Type: `Map EffectActionKey ActionEffectMap`
- Maps action keys to their effect configurations
- Manages what effects an action produces

### SystemEffectRegistry  
- Type: `Map SystemEffectKey (Map (GID SystemEffect) SystemEffectConfig)`
- Maps system-level effect keys to their configurations
- Handles system-wide effects

### SystemEffectKeysRegistry
- Type: `Map EffectActionKey [SystemEffectKey]`
- Maps action keys to lists of system effect keys
- Links actions to system effects

### TriggerRegistry
- Type: `Map EffectActionKey [(SystemEffectKey, GID SystemEffect, SystemEffectConfig)]`
- Maps actions to triggered system effects
- Manages cause-and-effect chains

## Relationship to Core Abstractions
- Registries implement the lock and meter patterns
- They track which actions affect which other actions (locks)
- They manage quantitative state changes (meters)

## Registries as Feature Extensions

### Pattern
Each registry adds a new capability to the game engine. Building out new registries is how features are added.

### Potential New Registries

#### Narration Manager
- Maps actions/events to narrative text
- Manages contextual descriptions
- Handles narrative variations

#### Temporal Effects Registry
- Maps actions to delayed consequences
- Tracks time-based state changes
- Manages effect scheduling and expiration
- Examples: poison that damages over time, buffs that expire, delayed triggers

#### Agent Management System
- Maps NPCs to behavior patterns
- Tracks agent states and goals
- Manages autonomous actions
- Handles dialogue trees and responses
- Examples: wandering merchants, guards with patrol routes, reactive NPCs

#### Threshold Registry
- Maps quantities to action availability
- Enforces minimum/maximum requirements
- Manages resource-gated actions
- Examples: need 10 gold to buy item, too encumbered to run, need 3 keys to open vault, too drunk to cast spell

#### Knowledge Management System
- Maps discovered information to capabilities
- Tracks what player has learned
- Gates actions based on knowledge
- See: docs/plans/knowledge-management.md for details

#### Navigation/Fast Travel Registry
- Maps discovered locations to paths
- Tracks solved obstacles along routes
- Enables direct travel to known locations
- Auto-solves intermediate puzzles when traversing
- Example: "GO TO LABORATORY" handles locked doors, puzzles if already solved

#### Undo/History Registry
- Tracks game state snapshots
- Maps actions to state transitions
- Enables rollback to previous states
- Could support branching timelines
- Manages save points and checkpoints

#### Counter Map
- Centralized numeric tracking for all systems
- Maps (System, Entity, Property) to values
- Handles all incrementing/decrementing/thresholds
- Backend for meters and resource tracking
- See: docs/plans/counter-map.md for details

## Implementation Details