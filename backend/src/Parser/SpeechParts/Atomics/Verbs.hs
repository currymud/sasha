module Parser.SpeechParts.Atomics.Verbs where

import           Data.Hashable (Hashable)
import           Data.HashSet  (HashSet, fromList, singleton)
import           Data.Kind     (Type)
import           Lexer

type Copula :: Type
newtype Copula = Copula { _fromCopula :: Lexeme }
  deriving newtype (Show, Eq, Hashable, Ord)

instance HasLexeme Copula where
  toLexeme = _fromCopula

copula :: HashSet Copula
copula = singleton $ Copula IS

type CardinalMovementVerb :: Type
newtype CardinalMovementVerb =
  CardinalMovementVerb { _fromCardinalMovementVerb :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord)

instance HasLexeme CardinalMovementVerb where
  toLexeme = _fromCardinalMovementVerb

cardinalMovementVerbs :: HashSet CardinalMovementVerb
cardinalMovementVerbs = fromList
  $ map CardinalMovementVerb [GO, SAIL, SNEAK,RUN , MARCH, FLOAT, FLEE, WALK]

type SpaceTransitionalVerb :: Type
newtype SpaceTransitionalVerb =
  SpaceTransitionalVerb { _fromSpaceTransitionalVerb :: Lexeme }
  deriving newtype (Show,Eq,Hashable, Ord)

instance HasLexeme SpaceTransitionalVerb where
  toLexeme = _fromSpaceTransitionalVerb

spaceTransitionalVerbs :: HashSet SpaceTransitionalVerb
spaceTransitionalVerbs =
  fromList $ map SpaceTransitionalVerb [CLIMB,CRAWL,SWIM,JUMP]

type ImplicitBoundaryVerb :: Type
newtype ImplicitBoundaryVerb =
  ImplicitBoundaryVerb { _fromImplicitBoundaryVerb :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord)

implicitBoundaryVerbs :: HashSet ImplicitBoundaryVerb
implicitBoundaryVerbs = singleton $ ImplicitBoundaryVerb EXIT

type ExplicitBoundaryVerb :: Type
newtype ExplicitBoundaryVerb =
  ExplicitBoundaryVerb { _fromExplicitBoundaryVerb :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord)

explicitBoundaryVerbs :: HashSet ExplicitBoundaryVerb
explicitBoundaryVerbs = singleton $ ExplicitBoundaryVerb ENTER

type ImplicitRegionalStimulusVerb :: Type
newtype ImplicitRegionalStimulusVerb =
  ImplicitRegionalStimulusVerb { _fromImplicitRegionalStimulusVerb :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord)

instance HasLexeme ImplicitRegionalStimulusVerb where
  toLexeme = _fromImplicitRegionalStimulusVerb
-- wait in line, sleep in bed
implicitRegionalStimulusVerbs :: HashSet ImplicitRegionalStimulusVerb
implicitRegionalStimulusVerbs = fromList $ map ImplicitRegionalStimulusVerb [WAIT, SLEEP]

type ImplicitStimulusVerb :: Type
newtype ImplicitStimulusVerb =
  ImplicitStimulusVerb { _fromImplicitStimulusVerb :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord)

instance HasLexeme ImplicitStimulusVerb where
  toLexeme = _fromImplicitStimulusVerb

implicitStimulusVerbs :: HashSet ImplicitStimulusVerb
implicitStimulusVerbs =
  fromList $ map ImplicitStimulusVerb [LOOK, SMELL, TASTE, LISTEN, TOUCH]

type ExplicitStimulusVerb :: Type
newtype ExplicitStimulusVerb =
  ExplicitStimulusVerb { _fromExplicitStimulusVerb :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord)

instance HasLexeme ExplicitStimulusVerb where
  toLexeme = _fromExplicitStimulusVerb

explicitStimulusVerbs :: HashSet ExplicitStimulusVerb
explicitStimulusVerbs = fromList $ map ExplicitStimulusVerb [SMELL,TASTE,TOUCH]

type DirectionalStimulusVerb :: Type
newtype DirectionalStimulusVerb = DirectionalStimulusVerb { _fromDirectionalStimulusVerb :: Lexeme }
  deriving newtype (Show, Eq, Hashable, Ord)

instance HasLexeme DirectionalStimulusVerb where
  toLexeme = _fromDirectionalStimulusVerb

-- Verbs that can take directional prepositions like "at"
directionalStimulusVerbs :: HashSet DirectionalStimulusVerb
directionalStimulusVerbs = singleton $ DirectionalStimulusVerb LOOK

type TargetedStimulusVerb :: Type
newtype TargetedStimulusVerb = TargetedStimulusVerb { _fromTargetedStimulusVerb :: Lexeme }
  deriving newtype (Show, Eq, Hashable, Ord)

instance HasLexeme TargetedStimulusVerb where
  toLexeme = _fromTargetedStimulusVerb

-- Verbs that can take targeted prepositions like "to"
targetedStimulusVerbs :: HashSet TargetedStimulusVerb
targetedStimulusVerbs = singleton $ TargetedStimulusVerb LISTEN
type TraversalVerb :: Type
newtype TraversalVerb = TraversalVerb { _fromTraversalVerb :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord)

instance HasLexeme TraversalVerb where
  toLexeme = _fromTraversalVerb

traversalVerbs :: HashSet TraversalVerb
traversalVerbs = fromList
  $ map TraversalVerb [MOVE, THROW,TOSS, WALK, PUSH]

type TraversalPathVerb :: Type
newtype TraversalPathVerb = TraversalPathVerb { _fromTraversalPathVerb :: Lexeme }
  deriving newtype (Show, Eq, Hashable, Ord)

instance HasLexeme TraversalPathVerb where
  toLexeme = _fromTraversalPathVerb

traversalPathVerbs :: HashSet TraversalPathVerb
traversalPathVerbs = singleton $ TraversalPathVerb SLIDE

type ToggleVerb :: Type
newtype ToggleVerb = ToggleVerb { _fromToggleVerb :: Lexeme }
  deriving newtype (Show, Eq, Hashable, Ord)

instance HasLexeme ToggleVerb where
  toLexeme = _fromToggleVerb

toggleVerbs :: HashSet ToggleVerb
toggleVerbs = fromList $ map ToggleVerb [FLIP, PUSH]

type ModToggleVerb :: Type
newtype ModToggleVerb = ModToggleVerb { _fromModToggleVerb :: Lexeme }
  deriving newtype (Show, Eq, Hashable, Ord)

instance HasLexeme ModToggleVerb where
  toLexeme = _fromModToggleVerb

modToggleVerbs :: HashSet ModToggleVerb
modToggleVerbs = singleton $ ModToggleVerb TURN

type SimpleAccessVerb :: Type
newtype SimpleAccessVerb = SimpleAccessVerb { _fromSimpleAccessVerb :: Lexeme }
  deriving newtype (Show, Eq, Hashable, Ord)

instance HasLexeme SimpleAccessVerb where
  toLexeme = _fromSimpleAccessVerb

-- These verbs don't take instruments
simpleAccessVerbs :: HashSet SimpleAccessVerb
simpleAccessVerbs = fromList $ map SimpleAccessVerb [OPEN, CLOSE]

type InstrumentalAccessVerb :: Type
newtype InstrumentalAccessVerb
  = InstrumentalAccessVerb { _fromInstrumentalAccessVerb :: Lexeme }
  deriving newtype (Show, Eq, Hashable, Ord)

instance HasLexeme InstrumentalAccessVerb where
  toLexeme = _fromInstrumentalAccessVerb

-- These verbs can take instruments (with "with" phrase)
instrumentalAccessVerbs :: HashSet InstrumentalAccessVerb
instrumentalAccessVerbs = fromList $ map InstrumentalAccessVerb [LOCK, UNLOCK]

type RotationalVerb :: Type
newtype RotationalVerb = RotationalVerb { _fromRotationalVerb :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord)
instance HasLexeme RotationalVerb where
  toLexeme = _fromRotationalVerb

rotationalVerbs :: HashSet RotationalVerb
rotationalVerbs = fromList $ map RotationalVerb [TURN, TWIST, ROTATE]

type DirectionalVerb :: Type
newtype DirectionalVerb = DirectionalVerb { _fromDirectionalVerb :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord)

instance HasLexeme DirectionalVerb where
  toLexeme = _fromDirectionalVerb

directionalVerbs :: HashSet DirectionalVerb
directionalVerbs = fromList $ map DirectionalVerb [PUSH, PULL]

type InstrumentActionVerb :: Type
newtype InstrumentActionVerb = InstrumentActionVerb { _fromInstrumentActionVerb :: Lexeme }
  deriving newtype (Show, Eq, Hashable, Ord)

instance HasLexeme InstrumentActionVerb where
  toLexeme = _fromInstrumentActionVerb

instrumentActionVerbs :: HashSet InstrumentActionVerb
instrumentActionVerbs = fromList $ map InstrumentActionVerb [PLAY, RING, SHOOT]

type InstrumentalPlacementVerb :: Type
newtype InstrumentalPlacementVerb = InstrumentalPlacementVerb { _fromInstrumentalPlacementVerb :: Lexeme }
  deriving newtype (Show, Eq, Hashable, Ord)

instrumentalPlacementVerbs :: HashSet InstrumentalPlacementVerb
instrumentalPlacementVerbs = fromList $ map InstrumentalPlacementVerb [PLANT,PUT,INSERT] -- Verbs that require/allow tools

instance HasLexeme InstrumentalPlacementVerb where
  toLexeme = _fromInstrumentalPlacementVerb

type GeneralPlacementVerb :: Type
newtype GeneralPlacementVerb = GeneralPlacementVerb { _fromGeneralPlacementVerb :: Lexeme }
  deriving newtype (Show, Eq, Hashable, Ord)

instance HasLexeme GeneralPlacementVerb where
  toLexeme = _fromGeneralPlacementVerb

generalPlacementVerbs :: HashSet GeneralPlacementVerb
generalPlacementVerbs = fromList $ map GeneralPlacementVerb [DROP, PUT] -- Verbs with minimal restrictions

type AcquisitionVerb :: Type
newtype AcquisitionVerb = AcquisitionVerb { _fromAcquisitionVerb :: Lexeme }
  deriving newtype (Show, Eq, Hashable, Ord)

instance HasLexeme AcquisitionVerb where
  toLexeme = _fromAcquisitionVerb

acquisitionVerbs :: HashSet AcquisitionVerb
acquisitionVerbs = fromList $ map AcquisitionVerb [GET,TAKE,REMOVE]

type TransferVerb :: Type
newtype TransferVerb = TransferVerb { _fromTransferVerb :: Lexeme }
  deriving newtype (Show, Eq, Hashable, Ord)

instance HasLexeme TransferVerb where
  toLexeme = _fromTransferVerb

transferVerbs :: HashSet TransferVerb
transferVerbs = singleton $ TransferVerb GIVE

type ResearchVerb :: Type
newtype ResearchVerb = ResearchVerb { _fromResearchVerb :: Lexeme }
  deriving newtype (Show, Eq, Hashable, Ord)

instance HasLexeme ResearchVerb where
  toLexeme = _fromResearchVerb

researchVerbs :: HashSet ResearchVerb
researchVerbs = singleton $ ResearchVerb LOOK
