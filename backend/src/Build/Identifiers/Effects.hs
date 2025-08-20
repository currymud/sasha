module Build.Identifiers.Effects (youSeeMEffectGID,systemEffectMap) where

import           Build.BedPuzzle.Effects.Perception.Look (youSeeMEffect)
import           Build.Templates.Identification          (makeSystemEffectGIDsAndMap)

makeSystemEffectGIDsAndMap
 [ ([| youSeeMEffect |], 1)]
