module Parser.SpeechParts.Atomics.Verbs where

import           Data.Hashable             (Hashable)
import           Data.HashSet              (HashSet, fromList, singleton)
import           Data.Kind                 (Type)
import           Lexer
import           Relude                    (ToText (toText))

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
  deriving newtype (Hashable)

instance HasLexeme Copula where
  toLexeme = _fromCopula

copula :: HashSet Copula
copula = singleton $ Copula IS

type CardinalMovementVerb :: Type
newtype CardinalMovementVerb =
  CardinalMovementVerb { _fromCardinalMovementVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme CardinalMovementVerb where
  toLexeme = _fromCardinalMovementVerb

cardinalMovementVerbs :: HashSet CardinalMovementVerb
cardinalMovementVerbs = fromList
  $ map CardinalMovementVerb [GO, SAIL, SNEAK,RUN , MARCH, FLOAT, FLEE, WALK]

type SpaceTransitionalVerb :: Type
newtype SpaceTransitionalVerb =
  SpaceTransitionalVerb { _fromSpaceTransitionalVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme SpaceTransitionalVerb where
  toLexeme = _fromSpaceTransitionalVerb

spaceTransitionalVerbs :: HashSet SpaceTransitionalVerb
spaceTransitionalVerbs =
  fromList $ map SpaceTransitionalVerb [CLIMB,CRAWL,SWIM,JUMP]

type ImplicitBoundaryVerb :: Type
newtype ImplicitBoundaryVerb =
  ImplicitBoundaryVerb { _fromImplicitBoundaryVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

implicitBoundaryVerbs :: HashSet ImplicitBoundaryVerb
implicitBoundaryVerbs = singleton $ ImplicitBoundaryVerb EXIT

type ExplicitBoundaryVerb :: Type
newtype ExplicitBoundaryVerb =
  ExplicitBoundaryVerb { _fromExplicitBoundaryVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

explicitBoundaryVerbs :: HashSet ExplicitBoundaryVerb
explicitBoundaryVerbs = singleton $ ExplicitBoundaryVerb ENTER

type ImplicitRegionalStimulusVerb :: Type
newtype ImplicitRegionalStimulusVerb =
  ImplicitRegionalStimulusVerb { _fromImplicitRegionalStimulusVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme ImplicitRegionalStimulusVerb where
  toLexeme = _fromImplicitRegionalStimulusVerb
-- wait in line, sleep in bed
implicitRegionalStimulusVerbs :: HashSet ImplicitRegionalStimulusVerb
implicitRegionalStimulusVerbs = fromList $ map ImplicitRegionalStimulusVerb [WAIT, SLEEP]

type ImplicitStimulusVerb :: Type
newtype ImplicitStimulusVerb =
  ImplicitStimulusVerb { _fromImplicitStimulusVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme ImplicitStimulusVerb where
  toLexeme = _fromImplicitStimulusVerb

implicitStimulusVerbs :: HashSet ImplicitStimulusVerb
implicitStimulusVerbs =
  fromList $ map ImplicitStimulusVerb [LOOK, SMELL, TASTE, LISTEN, TOUCH]

type ExplicitStimulusVerb :: Type
newtype ExplicitStimulusVerb =
  ExplicitStimulusVerb { _fromExplicitStimulusVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme ExplicitStimulusVerb where
  toLexeme = _fromExplicitStimulusVerb

explicitStimulusVerbs :: HashSet ExplicitStimulusVerb
explicitStimulusVerbs = fromList $ map ExplicitStimulusVerb [SMELL,TASTE,TOUCH]

type DirectionalStimulusVerb :: Type
newtype DirectionalStimulusVerb = DirectionalStimulusVerb { _fromDirectionalStimulusVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme DirectionalStimulusVerb where
  toLexeme = _fromDirectionalStimulusVerb

-- Verbs that can take directional prepositions like "at"
directionalStimulusVerbs :: HashSet DirectionalStimulusVerb
directionalStimulusVerbs = singleton $ DirectionalStimulusVerb LOOK

type TargetedStimulusVerb :: Type
newtype TargetedStimulusVerb = TargetedStimulusVerb { _fromTargetedStimulusVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme TargetedStimulusVerb where
  toLexeme = _fromTargetedStimulusVerb

-- Verbs that can take targeted prepositions like "to"
targetedStimulusVerbs :: HashSet TargetedStimulusVerb
targetedStimulusVerbs = singleton $ TargetedStimulusVerb LISTEN

type TraversalVerb :: Type
newtype TraversalVerb = TraversalVerb { _fromTraversalVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme TraversalVerb where
  toLexeme = _fromTraversalVerb

traversalVerbs :: HashSet TraversalVerb
traversalVerbs = fromList
  $ map TraversalVerb [MOVE, THROW,TOSS, WALK, PUSH]

type TraversalPathVerb :: Type
newtype TraversalPathVerb = TraversalPathVerb { _fromTraversalPathVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme TraversalPathVerb where
  toLexeme = _fromTraversalPathVerb

traversalPathVerbs :: HashSet TraversalPathVerb
traversalPathVerbs = singleton $ TraversalPathVerb SLIDE

type ToggleVerb :: Type
newtype ToggleVerb = ToggleVerb { _fromToggleVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme ToggleVerb where
  toLexeme = _fromToggleVerb

toggleVerbs :: HashSet ToggleVerb
toggleVerbs = fromList $ map ToggleVerb [FLIP, PUSH]

type ModToggleVerb :: Type
newtype ModToggleVerb = ModToggleVerb { _fromModToggleVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme ModToggleVerb where
  toLexeme = _fromModToggleVerb

modToggleVerbs :: HashSet ModToggleVerb
modToggleVerbs = singleton $ ModToggleVerb TURN

type SimpleAccessVerb :: Type
newtype SimpleAccessVerb = SimpleAccessVerb { _fromSimpleAccessVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme SimpleAccessVerb where
  toLexeme = _fromSimpleAccessVerb

-- These verbs don't take instruments
simpleAccessVerbs :: HashSet SimpleAccessVerb
simpleAccessVerbs = fromList $ map SimpleAccessVerb [OPEN, CLOSE]

type InstrumentalAccessVerb :: Type
newtype InstrumentalAccessVerb
  = InstrumentalAccessVerb { _fromInstrumentalAccessVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme InstrumentalAccessVerb where
  toLexeme = _fromInstrumentalAccessVerb

-- These verbs can take instruments (with "with" phrase)
instrumentalAccessVerbs :: HashSet InstrumentalAccessVerb
instrumentalAccessVerbs = fromList $ map InstrumentalAccessVerb [LOCK, UNLOCK]

type RotationalVerb :: Type
newtype RotationalVerb = RotationalVerb { _fromRotationalVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme RotationalVerb where
  toLexeme = _fromRotationalVerb

rotationalVerbs :: HashSet RotationalVerb
rotationalVerbs = fromList $ map RotationalVerb [TURN, TWIST, ROTATE]

type DirectionalVerb :: Type
newtype DirectionalVerb = DirectionalVerb { _fromDirectionalVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme DirectionalVerb where
  toLexeme = _fromDirectionalVerb

directionalVerbs :: HashSet DirectionalVerb
directionalVerbs = fromList $ map DirectionalVerb [PUSH, PULL]

type InstrumentActionVerb :: Type
newtype InstrumentActionVerb = InstrumentActionVerb { _fromInstrumentActionVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme InstrumentActionVerb where
  toLexeme = _fromInstrumentActionVerb

instrumentActionVerbs :: HashSet InstrumentActionVerb
instrumentActionVerbs = fromList $ map InstrumentActionVerb [PLAY, RING, SHOOT]

type InstrumentalPlacementVerb :: Type
newtype InstrumentalPlacementVerb = InstrumentalPlacementVerb { _fromInstrumentalPlacementVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instrumentalPlacementVerbs :: HashSet InstrumentalPlacementVerb
instrumentalPlacementVerbs = fromList $ map InstrumentalPlacementVerb [PLANT,PUT,INSERT] -- Verbs that require/allow tools

instance HasLexeme InstrumentalPlacementVerb where
  toLexeme = _fromInstrumentalPlacementVerb

type GeneralPlacementVerb :: Type
newtype GeneralPlacementVerb = GeneralPlacementVerb { _fromGeneralPlacementVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme GeneralPlacementVerb where
  toLexeme = _fromGeneralPlacementVerb

generalPlacementVerbs :: HashSet GeneralPlacementVerb
generalPlacementVerbs = fromList $ map GeneralPlacementVerb [DROP, PUT] -- Verbs with minimal restrictions

type AcquisitionVerb :: Type
newtype AcquisitionVerb = AcquisitionVerb { _fromAcquisitionVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme AcquisitionVerb where
  toLexeme = _fromAcquisitionVerb

acquisitionVerbs :: HashSet AcquisitionVerb
acquisitionVerbs = fromList $ map AcquisitionVerb [GET,TAKE,REMOVE]

type TransferVerb :: Type
newtype TransferVerb = TransferVerb { _fromTransferVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme TransferVerb where
  toLexeme = _fromTransferVerb

transferVerbs :: HashSet TransferVerb
transferVerbs = singleton $ TransferVerb GIVE

type ResearchVerb :: Type
newtype ResearchVerb = ResearchVerb { _fromResearchVerb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme ResearchVerb where
  toLexeme = _fromResearchVerb

researchVerbs :: HashSet ResearchVerb
researchVerbs = singleton $ ResearchVerb LOOK

#ifdef TESTING

newtype TestCopula = TestCopula {_fromTestCopula :: Text}
  deriving stock (Show, Eq, Ord)
  deriving newtype (Hashable)

toTestCopula :: Copula -> TestCopula
toTestCopula = TestCopula . toText

testCopula :: HashSet TestCopula
testCopula = fromList $ map toTestCopula $ HS.toList copula

instance ToText Copula where
  toText = toText . _fromCopula

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

instance ToText CardinalMovementVerb where
  toText = toText . _fromCardinalMovementVerb

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

instance ToText SpaceTransitionalVerb where
  toText = toText . _fromSpaceTransitionalVerb

instance Arbitrary ImplicitBoundaryVerb where
  arbitrary = elements $ HS.toList implicitBoundaryVerbs

instance ToText ImplicitBoundaryVerb where
  toText = toText . _fromImplicitBoundaryVerb

instance Arbitrary ExplicitBoundaryVerb where
  arbitrary = elements $ HS.toList explicitBoundaryVerbs

instance ToText ExplicitBoundaryVerb where
  toText = toText . _fromExplicitBoundaryVerb

instance Arbitrary ImplicitRegionalStimulusVerb where
  arbitrary = elements $ HS.toList implicitRegionalStimulusVerbs

instance ToText ImplicitRegionalStimulusVerb where
  toText = toText . _fromImplicitRegionalStimulusVerb

instance Arbitrary ImplicitStimulusVerb where
  arbitrary = elements $ HS.toList implicitStimulusVerbs

instance ToText ImplicitStimulusVerb where
  toText = toText . _fromImplicitStimulusVerb

instance Arbitrary ExplicitStimulusVerb where
  arbitrary = elements $ HS.toList explicitStimulusVerbs

instance ToText ExplicitStimulusVerb where
  toText = toText . _fromExplicitStimulusVerb

instance Arbitrary DirectionalStimulusVerb where
  arbitrary = elements $ HS.toList directionalStimulusVerbs

instance ToText DirectionalStimulusVerb where
  toText = toText . _fromDirectionalStimulusVerb

instance Arbitrary TargetedStimulusVerb where
  arbitrary = elements $ HS.toList targetedStimulusVerbs

instance ToText TargetedStimulusVerb where
  toText = toText . _fromTargetedStimulusVerb

instance Arbitrary TraversalVerb where
  arbitrary = elements $ HS.toList traversalVerbs

instance ToText TraversalVerb where
  toText = toText . _fromTraversalVerb

instance Arbitrary TraversalPathVerb where
  arbitrary = elements $ HS.toList traversalPathVerbs

instance ToText TraversalPathVerb where
  toText = toText . _fromTraversalPathVerb

instance Arbitrary ToggleVerb where
  arbitrary = elements $ HS.toList toggleVerbs

instance ToText ToggleVerb where
  toText = toText . _fromToggleVerb

instance Arbitrary ModToggleVerb where
  arbitrary = elements $ HS.toList modToggleVerbs

instance ToText ModToggleVerb where
  toText = toText . _fromModToggleVerb

instance Arbitrary SimpleAccessVerb where
  arbitrary = elements $ HS.toList simpleAccessVerbs

instance ToText SimpleAccessVerb where
  toText = toText . _fromSimpleAccessVerb

instance Arbitrary InstrumentalAccessVerb where
  arbitrary = elements $ HS.toList instrumentalAccessVerbs

instance ToText InstrumentalAccessVerb where
  toText = toText . _fromInstrumentalAccessVerb

instance Arbitrary RotationalVerb where
  arbitrary = elements $ HS.toList rotationalVerbs

instance ToText RotationalVerb where
  toText = toText . _fromRotationalVerb

instance Arbitrary DirectionalVerb where
  arbitrary = elements $ HS.toList directionalVerbs

instance ToText DirectionalVerb where
  toText = toText . _fromDirectionalVerb

instance Arbitrary InstrumentActionVerb where
  arbitrary = elements $ HS.toList instrumentActionVerbs

instance ToText InstrumentActionVerb where
  toText = toText . _fromInstrumentActionVerb

instance Arbitrary InstrumentalPlacementVerb where
  arbitrary = elements $ HS.toList instrumentalPlacementVerbs

instance ToText InstrumentalPlacementVerb where
  toText = toText . _fromInstrumentalPlacementVerb

instance Arbitrary GeneralPlacementVerb where
  arbitrary = elements $ HS.toList generalPlacementVerbs

instance ToText GeneralPlacementVerb where
  toText = toText . _fromGeneralPlacementVerb

instance Arbitrary AcquisitionVerb where
  arbitrary = elements $ HS.toList acquisitionVerbs

instance ToText AcquisitionVerb where
  toText = toText . _fromAcquisitionVerb

instance Arbitrary TransferVerb where
  arbitrary = elements $ HS.toList transferVerbs

instance ToText TransferVerb where
  toText = toText . _fromTransferVerb

instance Arbitrary ResearchVerb where
  arbitrary = elements $ HS.toList researchVerbs

instance ToText ResearchVerb where
  toText = toText . _fromResearchVerb
#endif

