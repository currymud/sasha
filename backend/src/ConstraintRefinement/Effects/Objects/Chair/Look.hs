module ConstraintRefinement.Effects.Objects.Chair.Look where

import Model.Core (Effect (NarrationEffect), NarrationComputation (LookAtNarration, ContainerContentsNarration, StaticNarration), Object)
import Model.GID (GID)

chairLookNarrationEffect :: GID Object -> Effect
chairLookNarrationEffect chairGID = NarrationEffect (LookAtNarration chairGID)

chairContentsNarrationEffect :: GID Object -> Effect
chairContentsNarrationEffect chairGID = NarrationEffect (ContainerContentsNarration chairGID)

-- From whatChairF action
whatChairEffect :: Effect
whatChairEffect = NarrationEffect (StaticNarration "It would be alot easier to see the chair if you would open your eyes. Literally.")