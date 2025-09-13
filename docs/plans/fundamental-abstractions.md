# Fundamental Abstractions

## Overview
Two fundamental abstractions have been identified for this problem domain:
1. **Lock** - 
2. **Meter** - 

## The Lock Abstraction

### Definition
A lock describes the relationship between actions. The simplest form is binary: one action either locks (disables) or unlocks (enables) another action.

### Properties
- Describes action-to-action relationships
- Simplest form: binary enable/disable
- Can potentially have more complex relationship patterns

### Types
- **Simple lock**: Binary state (locked/unlocked)
- **Unordered sequence lock**: Multiple conditions must be met, but order doesn't matter
- **Ordered sequence**: Just a chain of simple locks (A unlocks B, B unlocks C, etc.)

### Use Cases
- Simple lock: "open eyes" unlocks looking actions
- Simple lock: "get robe" affects availability of other actions
- Unordered sequence: Babel Fish puzzle - multiple preparatory actions required in any order before final action succeeds

## The Meter Abstraction

### Definition
A meter describes the relationship between a quantity and an action. It tracks numeric values that trigger or affect actions when certain thresholds are reached.

### Properties
- Tracks quantities/numeric values
- Triggers actions at thresholds
- Can count up or down
- Time-based or event-based

### Types
- **Time meters**: Count turns/time units
- **Quantity meters**: Track amounts of things (health, items, resources)

### Use Cases
- Timer: "five turns before the bomb goes off"
- Timer with reward: "in 5 turns the safe unlocks" (survive the dog, get the money)
- Resource depletion
- Accumulation triggers

## Relationship Between Lock and Meter

### Composition
Locks and meters can be combined to create complex game mechanics:
- A meter can control a lock (timer expires → lock changes state)
- A lock can gate access to meter-related actions
- Multiple locks and meters can interact in chains

### Example Combinations
- "Five turns to open the locked door to escape the bomb"
  - Meter: countdown timer (5 turns)
  - Lock: door is locked, must be unlocked
  - Consequence: bomb goes off if meter reaches zero before lock is opened

## Abstract Puzzle Mechanics

### Core Lock Patterns
1. **Simple lock**: One action changes another's availability
2. **Unordered sequence lock**: Multiple conditions, any order (Babel Fish)
3. **Chained locks**: Ordered sequence (A→B→C)
4. **Knowledge lock**: Learning permanently unlocks capabilities
5. **Abstraction lock**: Completing sequence unlocks meta-command
6. **Aggregate lock**: All conditions must be true simultaneously (collect all items)

### Core Meter Patterns  
1. **Countdown**: Decrements to trigger
2. **Accumulation**: Increments to trigger
3. **Maintenance**: Keep within range
4. **Oscillation**: Periodic state changes

### Combinations
1. **Meter → Lock**: Timer expires, state changes
2. **Lock → Meter**: Unlock starts/stops meter
3. **Parallel meters**: Multiple timers running
4. **Conditional meters**: Meter only runs when lock state allows

## Design Philosophy

### Negating Repetition (Plotkin's Innovation)
- First completion of multi-step process creates shortcut
- Player progression measured by abstraction level, not just inventory
- Reduces tedium while maintaining complexity
- Enables deeper puzzles without frustration

## Implementation Considerations