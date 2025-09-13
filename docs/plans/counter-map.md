# Counter Map System

## Overview
A centralized registry for tracking all numeric values across different game systems.

## Purpose
- Different systems need to track different quantities
- Centralizes all counting/metering logic
- Provides uniform interface for incrementing/decrementing/checking values

## Counter Types

### Turn Counters
- Global turn count
- Per-agent turn counts
- Turns since event

### Agent Counters
- Behavior cooldowns
- Action counts
- State durations

### Effect Counters
- Temporal effect durations
- Buff/debuff timers
- Delayed trigger countdowns

### Resource Counters
- Inventory quantities
- Health/mana/stamina
- Currency amounts

### Puzzle Counters
- Progress meters
- Combination locks
- Sequence trackers

## Counter Operations
- Increment/decrement
- Set/reset
- Check threshold
- Compare values
- Expire at zero

## Integration with Other Systems

### With Meters
- Counter Map provides the numeric backend
- Meters define the semantic meaning
- Thresholds trigger state changes

### With Locks
- Counters reaching thresholds can trigger locks
- "Need 5 keys" checks counter value
- "Too tired" when stamina counter too low

### With Agents
- Track per-agent statistics
- Manage behavior timers
- Count interactions

## Implementation Structure
```
CounterMap = Map CounterKey Integer
CounterKey = (System, Entity, Property)
```

Examples:
- (Turn, Global, Count) -> 152
- (Agent, Lenny, LastBeep) -> 3 
- (Effect, PoisonDebuff_123, Duration) -> 5
- (Resource, Player, Gold) -> 47