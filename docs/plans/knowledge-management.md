# Knowledge Management System

## Overview
A registry system for tracking what the player knows and how that knowledge affects available actions. Inspired by systems like Hadean Lands where knowledge fundamentally changes the scale of player interaction.

## Core Concepts

### Knowledge as Lock/Unlock Mechanism
- Learning unlocks new capabilities permanently
- Knowledge acts as a persistent lock state change
- Once learned, knowledge enables meta-actions

### Knowledge States
- Unknown: Action/information not available
- Discovered: Player has encountered but not mastered
- Learned: Player has full knowledge, can use shortcuts
- Automated: Complex sequences reduced to single actions

### Knowledge Types
- **Formulas/Rituals**: Complex procedures that can be automated once learned
- **Facts**: Information that gates dialogue or perception
- **Solutions**: Puzzle solutions that can be re-applied
- **Locations**: Places that can be navigated to directly once discovered

## Action Abstraction Through Knowledge

### Scale Shift Pattern (Hadean Lands style)
1. Initial interaction: Manual step-by-step process
2. Learning phase: Player discovers the sequence
3. Knowledge registration: System records the solution
4. Abstraction: Multi-step process becomes single command
   - Example: "CREATE HEALING POTION" replaces dozens of steps

### Goal-Seeking Integration
- Knowledge enables higher-level commands
- System can auto-solve known intermediate steps
- Player operates at progressively higher abstraction levels

## Interaction with Other Systems

### Knowledge → Actions
- Unlocks new verb phrases
- Enables meta-commands for complex sequences
- Gates access to advanced interactions

### Knowledge → Dialogue
- Changes available conversation options
- Enables informed responses
- Unlocks new quest branches

### Knowledge → Perception
- Reveals hidden details in descriptions
- Changes how objects are understood
- Enables pattern recognition

## Implementation Considerations

### Knowledge Persistence
- Survives resets/restarts (Groundhog Day pattern)
- Stored in Knowledge Registry
- Maps knowledge keys to capability sets

### Knowledge Discovery
- Triggered by actions, observations, dialogue
- Can be gradual (partial knowledge) or instant
- May require combinations of facts

### Knowledge Dependencies
- Some knowledge requires prerequisites
- Knowledge graphs for complex relationships
- Cascading unlocks when learning key facts