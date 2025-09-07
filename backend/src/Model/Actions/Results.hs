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
import           Data.Text                     (Text)
import           Model.Core                    (EffectActionKey,
                                                GameComputation)
import           Model.GID                     (GID)
import           Model.Parser.Composites.Nouns (ContainerPhrase,
                                                InstrumentalAccessNounPhrase,
                                                ObjectPhrase, SupportPhrase)
import           Model.Parser.GCase            (NounKey)

-- | Coordination result - bundles computation with effects after constraint solving
data CoordinationResult = CoordinationResult
  { _computation      :: GameComputation Identity ()
  , _actionEffectKeys :: [EffectActionKey]
  , _fieldEffectKeys  :: [EffectActionKey]
  }

-- | Container access coordination result
data ContainerAccessResult = ContainerAccessResult
  { _containerActionEffectKeys :: [EffectActionKey]
  , _containerFieldEffectKeys  :: [EffectActionKey]
  }
  deriving stock (Show, Eq, Ord)

-- | Instrument access coordination result
data InstrumentAccessResult = InstrumentAccessResult
  { _instrumentActionEffectKeys :: [EffectActionKey]
  , _instrumentFieldEffectKeys  :: [EffectActionKey]
  }
  deriving stock (Show, Eq, Ord)

-- | Acquisition parsing result - Simple vs Complete acquisition commands
data AcquisitionRes
  = Complete CompleteAcquisitionRes
  | Simple SimpleAcquisitionRes
  deriving stock (Show, Eq, Ord)

-- | Complete acquisition: "get X from Y"
data CompleteAcquisitionRes = CompleteAcquisitionRes
  { _caObjectKey     :: NounKey
  , _caObjectPhrase  :: ObjectPhrase
  , _caSupportKey    :: NounKey
  , _caSupportPhrase :: SupportPhrase
  }
  deriving stock (Show, Eq, Ord)

-- | Simple acquisition: "get X"
data SimpleAcquisitionRes = SimpleAcquisitionRes
  { _saObjectKey    :: NounKey
  , _saObjectPhrase :: ObjectPhrase
  }
  deriving stock (Show, Eq, Ord)

-- | Access parsing result - Simple vs Complete access commands
data AccessRes
  = CompleteAR CompleteAccessRes
  | SimpleAR SimpleAccessRes
  deriving stock (Show, Eq, Ord)

-- | Complete access: "open X with Y"
data CompleteAccessRes = CompleteAccessRes
  { _containerKey     :: NounKey
  , _ContainerPhrase  :: ContainerPhrase
  , _instrumentKey    :: NounKey
  , _instrumentPhrase :: InstrumentalAccessNounPhrase
  }
  deriving stock (Show, Eq, Ord)

-- | Simple access: "open X"
data SimpleAccessRes = SimpleAccessRes
  { _saContainerKey    :: NounKey
  , _saContainerPhrase :: ContainerPhrase
  }
  deriving stock (Show, Eq, Ord)
