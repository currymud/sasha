# Sasha Development ToDo

## Priority Tasks

### 1. Narration Management System
**Status**: In Progress - Code cleanup phase
- [ ] Complete code cleanup and standardization
- [ ] Design narration management architecture
- [ ] Remove all hard-coded narrations from action handlers
- [ ] Implement centralized narration generation
- [ ] Create narration templates/patterns
- [ ] Integrate with effect system

### 2. Dependency Map
**Status**: Planned - After narration system
- [ ] Create comprehensive module dependency visualization
- [ ] Document core type dependencies (Model.Core usage)
- [ ] Map GameState function dependencies
- [ ] Track effect registry access patterns
- [ ] Identify circular dependencies if any
- [ ] Create quick reference for "where to add X" questions

## Notes
- Current focus: Cleaning up code to prepare for narration management system
- Hard-coded narrations are scattered throughout ConstraintRefinement actions
- Need consistent patterns before implementing narration system
- Dependency map will help manage growing codebase complexity

## Completed
- [x] Rename ActionEffectKey to TargetEffectKey
- [x] Rename EffectActionKey to ActionEffectKey
- [x] Update failure actions to receive ActionEffectKey (e.g., getDeniedF)