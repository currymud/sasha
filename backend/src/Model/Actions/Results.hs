{-# LANGUAGE DerivingStrategies #-}

module Model.Actions.Results
  ( -- * Constraint Resolution Results
    CoordinationResult(..)
  , ContainerAccessResult(..)
  , InstrumentAccessResult(..)
    -- * Parsed Command Results
  , AcquisitionRes(..)
  , CompleteAcquisitionRes(..)
  , SimpleAcquisitionRes(..)
  , AccessRes(..)
  , CompleteAccessRes(..)
  , SimpleAccessRes(..)
  ) where

import           Control.Monad.Identity        (Identity)
import           Data.Kind                     (Type)
import           Model.Core                    (ActionEffectKey,
                                                GameComputation)
import           Model.Parser.Composites.Nouns (ContainerPhrase,
                                                InstrumentalAccessNounPhrase,
                                                ObjectPhrase, SupportPhrase)
import           Model.Parser.GCase            (NounKey)

-- | Coordination result - bundles computation with effects after constraint solving

type CoordinationResult :: Type
data CoordinationResult = CoordinationResult
  { _computation      :: GameComputation Identity ()
  , _actionEffectKeys :: [ActionEffectKey]
  , _fieldEffectKeys  :: [ActionEffectKey]
  }

-- | Container access coordination result
type ContainerAccessResult :: Type
data ContainerAccessResult = ContainerAccessResult
  { _containerActionEffectKeys :: [ActionEffectKey]
  , _containerFieldEffectKeys  :: [ActionEffectKey]
  }
  deriving stock (Show, Eq, Ord)

-- | Instrument access coordination result
type InstrumentAccessResult :: Type
data InstrumentAccessResult = InstrumentAccessResult
  { _instrumentActionEffectKeys :: [ActionEffectKey]
  , _instrumentFieldEffectKeys  :: [ActionEffectKey]
  }
  deriving stock (Show, Eq, Ord)

-- | Acquisition parsing result - Simple vs Complete acquisition commands
type AcquisitionRes :: Type
data AcquisitionRes
  = Complete CompleteAcquisitionRes
  | Simple SimpleAcquisitionRes
  deriving stock (Show, Eq, Ord)

-- | Complete acquisition: "get X from Y"
type CompleteAcquisitionRes :: Type
data CompleteAcquisitionRes = CompleteAcquisitionRes
  { _caObjectKey     :: NounKey
  , _caObjectPhrase  :: ObjectPhrase
  , _caSupportKey    :: NounKey
  , _caSupportPhrase :: SupportPhrase
  }
  deriving stock (Show, Eq, Ord)

-- | Simple acquisition: "get X"
type SimpleAcquisitionRes :: Type
data SimpleAcquisitionRes = SimpleAcquisitionRes
  { _saObjectKey    :: NounKey
  , _saObjectPhrase :: ObjectPhrase
  }
  deriving stock (Show, Eq, Ord)

-- | Access parsing result - Simple vs Complete access commands
type AccessRes :: Type
data AccessRes
  = CompleteAR CompleteAccessRes
  | SimpleAR SimpleAccessRes
  deriving stock (Show, Eq, Ord)

-- | Complete access: "open X with Y"
type CompleteAccessRes :: Type
data CompleteAccessRes = CompleteAccessRes
  { _containerKey     :: NounKey
  , _ContainerPhrase  :: ContainerPhrase
  , _instrumentKey    :: NounKey
  , _instrumentPhrase :: InstrumentalAccessNounPhrase
  }
  deriving stock (Show, Eq, Ord)

-- | Simple access: "open X"
type SimpleAccessRes :: Type
data SimpleAccessRes = SimpleAccessRes
  { _saContainerKey    :: NounKey
  , _saContainerPhrase :: ContainerPhrase
  }
  deriving stock (Show, Eq, Ord)
