# Aggregate Puzzles

## Core Concept
Puzzles that require all conditions in a set to be true simultaneously. This pattern captures both simple collection puzzles and complex preparation puzzles like the Babel Fish.

## Demo Room Example
Door unlocks only when all items in room are in inventory.

### Registry Structure
```
data WillGo = Go | NoGo

AggregateRegistry = Map ItemID (GameComputation Identity WillGo)
```
- Each key maps to an action that returns Go or NoGo
- Taking item → action returns Go
- Dropping item → action returns NoGo
- Actions can themselves produce effects

### Action Flow

1. **Player tries "open door"**
   - Action fires effect
   - Effect applies management function to registry

2. **Management Function**
   - Executes all mapped actions: `traverse id (Map.elems registry)`
   - Each action may produce effects
   - Returns: (effects, [WillGo])
   - If all Go → unlock door
   - If any NoGo → door remains locked

3. **Success State**
   - All items collected (all Go)
   - Accumulated effects fire
   - "leave room" becomes available

### Why Functions Over Simple Values
- Each check can have side effects
- Checking an item might trigger descriptions
- Some conditions might need complex computation
- Maintains dynamic dispatch pattern
- **NoGo results add narration to explain what's missing**

### Narration Integration
When the management function runs:
- Go results: silent success
- NoGo results: contribute failure messages to Narration Effect system
- Example: "The towel isn't covering the drain" + "The robe isn't on the hook"
- Player gets comprehensive feedback about what still needs to be done

## System Interactions

### With Effect System
- "take item" effect updates registry to Right
- "drop item" effect updates registry to Left
- "open door" effect queries registry state

### With Lock System
- Aggregate lock pattern
- Binary state based on collection completeness
- Dynamic - can lock/unlock repeatedly

### With Action System
- "open door" → triggers registry check
- Registry success → enables "leave room"
- "leave room" was previously locked

## Other Aggregate Puzzle Examples

### Babel Fish Puzzle
- All preparations must be complete: robe on hook, towel over drain, mail on satchel, etc.
- Each preparation: computation returns Go (done) / NoGo (not done)
- Fish dispenser works when all Go
- Order doesn't matter - classic unordered sequence lock implemented as aggregate

### Ritual Preparation
- All candles must be lit
- Each candle: computation checks state, returns Go/NoGo
- Complete ritual when all Go

### Machine Assembly
- All parts must be installed  
- Each part: computation verifies installation, returns Go/NoGo
- Machine activates when all Go

### Combination Lock
- All tumblers in correct position
- Each tumbler: computation checks position, returns Go/NoGo
- Lock opens when all Go

### Multiplayer Coordination
- All players on pressure plates
- Each position: computation checks occupancy, returns Go/NoGo
- Door opens when all Go

## Implementation Pattern
The WillGo/NoGo pattern with functions:
- More expressive than Either - domain-specific type
- Each check is a computation that can produce effects
- Maintains dynamic dispatch consistency
- Avoids explicit state machine in favor of functional composition
- Management function aggregates both effects and decisions
- Pattern: `Map Key (GameComputation Identity WillGo)`

This is a controlled introduction of state-like behavior while maintaining the elegance of dynamic dispatch and effect composition.