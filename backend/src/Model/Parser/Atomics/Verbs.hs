module Model.Parser.Atomics.Verbs where

import           Data.Hashable             (Hashable)
import           Data.HashSet              (HashSet, fromList, singleton)
import           Data.Kind                 (Type)
import           Lexer
import           Relude                    (ToText)

#ifdef TESTING
import qualified Data.HashSet              as HS
import           Data.Text                 (Text)
import           Relude.String.Conversion  (ToText (toText))
import           Test.QuickCheck           (Arbitrary, elements)
import           Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
#endif

type Copula :: Type
newtype Copula = Copula { _fromCopula :: Lexeme }
  deriving stock (Show, Eq, Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme Copula where
  toLexeme = _fromCopula

type CardinalMovementVerb :: Type
newtype CardinalMovementVerb =
  CardinalMovementVerb { _fromCardinalMovementVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme CardinalMovementVerb where
  toLexeme = _fromCardinalMovementVerb

type SpaceTransitionalVerb :: Type
newtype SpaceTransitionalVerb =
  SpaceTransitionalVerb { _fromSpaceTransitionalVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme SpaceTransitionalVerb where
  toLexeme = _fromSpaceTransitionalVerb

type ImplicitBoundaryVerb :: Type
newtype ImplicitBoundaryVerb =
  ImplicitBoundaryVerb { _fromImplicitBoundaryVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme ImplicitBoundaryVerb where
  toLexeme = _fromImplicitBoundaryVerb

type ExplicitBoundaryVerb :: Type
newtype ExplicitBoundaryVerb =
  ExplicitBoundaryVerb { _fromExplicitBoundaryVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme ExplicitBoundaryVerb where
  toLexeme = _fromExplicitBoundaryVerb

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

type ExplicitStimulusVerb :: Type
newtype ExplicitStimulusVerb =
  ExplicitStimulusVerb { _fromExplicitStimulusVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme ExplicitStimulusVerb where
  toLexeme = _fromExplicitStimulusVerb

type DirectionalStimulusVerb :: Type
newtype DirectionalStimulusVerb = DirectionalStimulusVerb { _fromDirectionalStimulusVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme DirectionalStimulusVerb where
  toLexeme = _fromDirectionalStimulusVerb

type TargetedStimulusVerb :: Type
newtype TargetedStimulusVerb = TargetedStimulusVerb { _fromTargetedStimulusVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme TargetedStimulusVerb where
  toLexeme = _fromTargetedStimulusVerb

type TraversalVerb :: Type
newtype TraversalVerb = TraversalVerb { _fromTraversalVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme TraversalVerb where
  toLexeme = _fromTraversalVerb

type TraversalPathVerb :: Type
newtype TraversalPathVerb = TraversalPathVerb { _fromTraversalPathVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme TraversalPathVerb where
  toLexeme = _fromTraversalPathVerb

type ToggleVerb :: Type
newtype ToggleVerb = ToggleVerb { _fromToggleVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme ToggleVerb where
  toLexeme = _fromToggleVerb

type ModToggleVerb :: Type
newtype ModToggleVerb = ModToggleVerb { _fromModToggleVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme ModToggleVerb where
  toLexeme = _fromModToggleVerb

type SimpleAccessVerb :: Type
newtype SimpleAccessVerb = SimpleAccessVerb { _fromSimpleAccessVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme SimpleAccessVerb where
  toLexeme = _fromSimpleAccessVerb

type InstrumentalAccessVerb :: Type
newtype InstrumentalAccessVerb
  = InstrumentalAccessVerb { _fromInstrumentalAccessVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme InstrumentalAccessVerb where
  toLexeme = _fromInstrumentalAccessVerb

type RotationalVerb :: Type
newtype RotationalVerb = RotationalVerb { _fromRotationalVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme RotationalVerb where
  toLexeme = _fromRotationalVerb

type DirectionalVerb :: Type
newtype DirectionalVerb = DirectionalVerb { _fromDirectionalVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme DirectionalVerb where
  toLexeme = _fromDirectionalVerb

type InstrumentActionVerb :: Type
newtype InstrumentActionVerb = InstrumentActionVerb { _fromInstrumentActionVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme InstrumentActionVerb where
  toLexeme = _fromInstrumentActionVerb

type InstrumentalPlacementVerb :: Type
newtype InstrumentalPlacementVerb = InstrumentalPlacementVerb { _fromInstrumentalPlacementVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme InstrumentalPlacementVerb where
  toLexeme = _fromInstrumentalPlacementVerb

type GeneralPlacementVerb :: Type
newtype GeneralPlacementVerb = GeneralPlacementVerb { _fromGeneralPlacementVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme GeneralPlacementVerb where
  toLexeme = _fromGeneralPlacementVerb

type AcquisitionVerb :: Type
newtype AcquisitionVerb = AcquisitionVerb { _fromAcquisitionVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme AcquisitionVerb where
  toLexeme = _fromAcquisitionVerb

type TransferVerb :: Type
newtype TransferVerb = TransferVerb { _fromTransferVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme TransferVerb where
  toLexeme = _fromTransferVerb

type ResearchVerb :: Type
newtype ResearchVerb = ResearchVerb { _fromResearchVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme ResearchVerb where
  toLexeme = _fromResearchVerb

#ifdef TESTING

newtype TestCopula = TestCopula {_fromTestCopula :: Text}
  deriving stock (Show, Eq, Ord)
  deriving newtype (Hashable)

toTestCopula :: Copula -> TestCopula
toTestCopula = TestCopula . toText

testCopula :: HashSet TestCopula
testCopula = fromList $ map toTestCopula $ HS.toList copula

instance Arbitrary TestCopula where
  arbitrary = elements $ HS.toList testCopula

instance Arbitrary Copula where
  arbitrary = elements $ HS.toList copula

newtype TestCardinalMovementVerb = TestCardinalMovementVerb {_fromTestCardinalMovementVerb :: Text}
  deriving stock (Show, Eq, Ord)
  deriving newtype (Hashable)

toTestCardinalMovementVerb :: CardinalMovementVerb -> TestCardinalMovementVerb
toTestCardinalMovementVerb = TestCardinalMovementVerb . toText

testCardinalMovementVerbs :: HashSet TestCardinalMovementVerb
testCardinalMovementVerbs = fromList $ map toTestCardinalMovementVerb $ HS.toList cardinalMovementVerbs

instance Arbitrary TestCardinalMovementVerb where
  arbitrary = elements $ HS.toList testCardinalMovementVerbs

instance Arbitrary CardinalMovementVerb where
  arbitrary = elements $ HS.toList cardinalMovementVerbs

newtype TestSpaceTransitionalVerb = TestSpaceTransitionalVerb {_fromTestSpaceTransitionalVerb :: Text}
  deriving stock (Show, Eq, Ord)
  deriving newtype (Hashable)

toTestSpaceTransitionalVerb :: SpaceTransitionalVerb -> TestSpaceTransitionalVerb
toTestSpaceTransitionalVerb = TestSpaceTransitionalVerb . toText

testSpaceTransitionalVerbs :: HashSet TestSpaceTransitionalVerb
testSpaceTransitionalVerbs = fromList $ map toTestSpaceTransitionalVerb $ HS.toList spaceTransitionalVerbs

instance Arbitrary TestSpaceTransitionalVerb where
  arbitrary = elements $ HS.toList testSpaceTransitionalVerbs

instance Arbitrary SpaceTransitionalVerb where
  arbitrary = elements $ HS.toList spaceTransitionalVerbs

instance Arbitrary ImplicitBoundaryVerb where
  arbitrary = elements $ HS.toList implicitBoundaryVerbs

instance Arbitrary ExplicitBoundaryVerb where
  arbitrary = elements $ HS.toList explicitBoundaryVerbs

instance Arbitrary ImplicitRegionalStimulusVerb where
  arbitrary = elements $ HS.toList implicitRegionalStimulusVerbs

instance Arbitrary ImplicitStimulusVerb where
  arbitrary = elements $ HS.toList implicitStimulusVerbs

instance Arbitrary ExplicitStimulusVerb where
  arbitrary = elements $ HS.toList explicitStimulusVerbs

instance Arbitrary DirectionalStimulusVerb where
  arbitrary = elements $ HS.toList directionalStimulusVerbs

instance Arbitrary TargetedStimulusVerb where
  arbitrary = elements $ HS.toList targetedStimulusVerbs

instance Arbitrary TraversalVerb where
  arbitrary = elements $ HS.toList traversalVerbs

instance Arbitrary TraversalPathVerb where
  arbitrary = elements $ HS.toList traversalPathVerbs

instance Arbitrary ToggleVerb where
  arbitrary = elements $ HS.toList toggleVerbs

instance Arbitrary ModToggleVerb where
  arbitrary = elements $ HS.toList modToggleVerbs

instance Arbitrary SimpleAccessVerb where
  arbitrary = elements $ HS.toList simpleAccessVerbs

instance Arbitrary InstrumentalAccessVerb where
  arbitrary = elements $ HS.toList instrumentalAccessVerbs

instance Arbitrary RotationalVerb where
  arbitrary = elements $ HS.toList rotationalVerbs

instance Arbitrary DirectionalVerb where
  arbitrary = elements $ HS.toList directionalVerbs

instance Arbitrary InstrumentActionVerb where
  arbitrary = elements $ HS.toList instrumentActionVerbs

instance Arbitrary InstrumentalPlacementVerb where
  arbitrary = elements $ HS.toList instrumentalPlacementVerbs

instance Arbitrary GeneralPlacementVerb where
  arbitrary = elements $ HS.toList generalPlacementVerbs

instance Arbitrary AcquisitionVerb where
  arbitrary = elements $ HS.toList acquisitionVerbs

instance Arbitrary TransferVerb where
  arbitrary = elements $ HS.toList transferVerbs

instance Arbitrary ResearchVerb where
  arbitrary = elements $ HS.toList researchVerbs
#endif

