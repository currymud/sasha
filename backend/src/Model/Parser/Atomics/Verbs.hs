module Model.Parser.Atomics.Verbs (AcquisitionVerb (AcquisitionVerb, _fromAcquisitionVerb)
                                  , CardinalMovementVerb (CardinalMovementVerb, _fromCardinalMovementVerb )
                                  , DirectionalStimulusVerb (DirectionalStimulusVerb, _fromDirectionalStimulusVerb)
                                  , EdibleConsumptionVerb (EdibleConsumptionVerb, _fromEdibleConsumptionVerb)
                                  , ImplicitBoundaryVerb (ImplicitBoundaryVerb, _fromImplicitBoundaryVerb)
                                  , ImplicitRegionalStimulusVerb (ImplicitRegionalStimulusVerb, _fromImplicitRegionalStimulusVerb)
                                  , ImplicitStimulusVerb (ImplicitStimulusVerb, _fromImplicitStimulusVerb)
                                  , InstrumentActionVerb (InstrumentActionVerb, _fromInstrumentActionVerb)
                                  , SimpleAccessVerb (SimpleAccessVerb, _fromSimpleAccessVerb)
                                  , SomaticAccessVerb (SomaticAccessVerb, _fromSomaticAccessVerb)
                                  , ) where

import           Data.Hashable        (Hashable)
import           Data.Kind            (Type)
import           Grammar.Parser.Lexer (HasLexeme (toLexeme), Lexeme)
import           Relude               (ToText)

type AcquisitionVerb :: Type
newtype AcquisitionVerb =
  AcquisitionVerb { _fromAcquisitionVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme AcquisitionVerb where
  toLexeme = _fromAcquisitionVerb

type CardinalMovementVerb :: Type
newtype CardinalMovementVerb =
  CardinalMovementVerb { _fromCardinalMovementVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme CardinalMovementVerb where
  toLexeme = _fromCardinalMovementVerb

type DirectionalStimulusVerb :: Type
newtype DirectionalStimulusVerb = DirectionalStimulusVerb { _fromDirectionalStimulusVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme DirectionalStimulusVerb where
  toLexeme = _fromDirectionalStimulusVerb

type EdibleConsumptionVerb :: Type
newtype EdibleConsumptionVerb =
  EdibleConsumptionVerb { _fromEdibleConsumptionVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme EdibleConsumptionVerb where
  toLexeme = _fromEdibleConsumptionVerb

type ImplicitBoundaryVerb :: Type
newtype ImplicitBoundaryVerb =
  ImplicitBoundaryVerb { _fromImplicitBoundaryVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme ImplicitBoundaryVerb where
  toLexeme = _fromImplicitBoundaryVerb

type ImplicitRegionalStimulusVerb :: Type
newtype ImplicitRegionalStimulusVerb =
  ImplicitRegionalStimulusVerb { _fromImplicitRegionalStimulusVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme ImplicitRegionalStimulusVerb where
  toLexeme = _fromImplicitRegionalStimulusVerb

type ImplicitStimulusVerb :: Type
newtype ImplicitStimulusVerb =
  ImplicitStimulusVerb { _fromImplicitStimulusVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme ImplicitStimulusVerb where
  toLexeme = _fromImplicitStimulusVerb

type InstrumentActionVerb :: Type
newtype InstrumentActionVerb =
  InstrumentActionVerb { _fromInstrumentActionVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme InstrumentActionVerb where
  toLexeme = _fromInstrumentActionVerb

type SimpleAccessVerb :: Type
newtype SimpleAccessVerb = SimpleAccessVerb { _fromSimpleAccessVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme SimpleAccessVerb where
  toLexeme = _fromSimpleAccessVerb

type SomaticAccessVerb :: Type
newtype SomaticAccessVerb =
  SomaticAccessVerb { _fromSomaticAccessVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme SomaticAccessVerb where
  toLexeme = _fromSomaticAccessVerb
