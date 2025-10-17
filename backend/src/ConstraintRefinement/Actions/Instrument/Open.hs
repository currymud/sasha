module ConstraintRefinement.Actions.Instrument.Open where

import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (InstrumentContainerAccessActionF (InstrumentCanAccessF, InstrumentCannotAccessF))

openInstrumentF :: InstrumentContainerAccessActionF
openInstrumentF = InstrumentCanAccessF processEffectsFromRegistry

openInstrumentDeniedF :: InstrumentContainerAccessActionF
openInstrumentDeniedF = InstrumentCannotAccessF processEffectsFromRegistry