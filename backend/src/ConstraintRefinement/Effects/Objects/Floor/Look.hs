module ConstraintRefinement.Effects.Objects.Floor.Look where

import Model.Core (Effect (NarrationEffect), NarrationComputation (LookAtNarration, ContainerContentsNarration, StaticNarration), Object)
import Model.GID (GID)

floorLookNarrationEffect :: GID Object -> Effect
floorLookNarrationEffect floorGID = NarrationEffect (LookAtNarration floorGID)

floorContentsNarrationEffect :: GID Object -> Effect
floorContentsNarrationEffect floorGID = NarrationEffect (ContainerContentsNarration floorGID)

-- From notEvenFloorF action
notEvenFloorEffect :: Effect
notEvenFloorEffect = NarrationEffect (StaticNarration "One step at a time champ. You had a rough night. Open your eyes first.")