module ConstraintRefinement.Effects.Objects.SupportLook where

import Model.Core (Effect (NarrationEffect), NarrationComputation (LookAtNarration, ContainerContentsNarration), Object)
import Model.GID (GID)

-- For dynamic support/container descriptions
supportLookNarrationEffect :: GID Object -> Effect
supportLookNarrationEffect objGID = NarrationEffect (LookAtNarration objGID)

supportContentsNarrationEffect :: GID Object -> Effect
supportContentsNarrationEffect objGID = NarrationEffect (ContainerContentsNarration objGID)