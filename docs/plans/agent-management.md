# Agent Management System

## Overview
A registry system for managing NPCs and other autonomous entities in the game world, including the player.

## Core Architecture Change

### Unifying Player and NPCs
- Remove special "Player" type
- Replace with generic "Agent" type
- Create AgentMap to track all agents
- Player becomes just another agent with special input handling

### Benefits of Unification
- Player actions and NPC actions use same system
- Enables body-swapping, possession mechanics
- Simplifies multiplayer potential
- Consistent rules for all entities

## Core Concepts

### AgentMap Structure
- Maps Agent IDs to Agent states
- Tracks which agent is player-controlled
- Manages turn order for all agents
- Handles agent spawning/removal

### Agents as Active Entities
- All agents (including player) can initiate actions
- Have their own goals and behaviors
- Can respond to other agents and world state
- Share same action system and constraints

### Agent States

### Behavior Patterns

## Reaction System

### Overview
- Tracks how agents respond to actions by other agents
- Part of the Agent Management System
- Defines agent-to-agent interaction patterns

### Reaction Registry
- Maps (Agent, Action, Target) → Response
- Can be conditional based on state
- Enables dynamic social interactions
- Examples: guard reacts to theft, merchant reacts to purchase, dog reacts to threat

### Reaction Types
- Immediate responses (dialogue, action)
- State changes (friendship, hostility)
- Triggered behaviors (flee, attack, help)

## Interaction with Lock/Meter System

### Agent-Triggered Locks
- Agent reactions can lock/unlock player actions
- Social locks: "befriend guard to unlock area"

### Agent-Controlled Meters
- Reputation meters per agent
- Relationship tracking
- Trust/fear/respect values

## Agent Types

### Player Agent
- Receives input from user
- Otherwise identical to other agents

### Autonomous Agents
- NPCs with behaviors triggered each turn
- Act regardless of player's specific action

## Autonomous Behavior System

### Turn-Based Autonomy
- Every turn, each agent gets to act
- "Independence" = agent acts without caring about player's specific action
- Agent behaviors triggered by turn counter, not player actions

### Autonomous Behaviors
- **Turn-triggered actions**: Every turn, agent may do something
- **Probabilistic actions**: Random chance each turn
- **Cyclic behaviors**: Rotate through action patterns
- **Ambient actions**: Flavor text/emotes on some turns

### Example: Lenny the Robot
- Location: Outside door
- Each turn Lenny might:
  - Emit whirring sounds (30% chance)
  - Adjust sensors (20% chance)  
  - Run diagnostic routine (10% chance)
  - Do nothing (40% chance)
- Also has reactions to specific player actions
- Both systems work in parallel:
  - Turn happens → Lenny's autonomous behavior fires
  - If player acted toward Lenny → reaction also fires

## Implementation Considerations

### Turn Management
- Turn-based: nothing happens between turns
- Each turn:
  1. Player action processes
  2. All agents' autonomous behaviors process
  3. All reactions process
  4. World state updates

### Decision Making
- Random selection from behavior pool
- Can be weighted by state
- Independent of player action choice

### State Persistence
- Track turns since last behavior
- Cooldowns in turn counts
- Behavior history for variety