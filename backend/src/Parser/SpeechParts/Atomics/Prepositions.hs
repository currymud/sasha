module Parser.SpeechParts.Atomics.Prepositions where

import           Data.Hashable             (Hashable)
import           Data.HashSet              (HashSet, fromList, singleton,
                                            toList)
import           Data.Kind                 (Type)
import           Lexer

#ifdef TESTING
import qualified Data.HashSet              as HS
import           Relude.String.Conversion  (ToText (toText))
import           Test.QuickCheck           (Arbitrary, elements)
import           Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
#endif

type LocationInterrogativeMarker :: Type
newtype LocationInterrogativeMarker =
  LocationInterrogativeMarker { _fromLocationInterrogativeMarker :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme LocationInterrogativeMarker where
  toLexeme = _fromLocationInterrogativeMarker

locationInterrogativeMarker :: HashSet LocationInterrogativeMarker
locationInterrogativeMarker = singleton $ LocationInterrogativeMarker WHERE

-- For "what about" constructions
type ObjectInterrogativeMarker :: Type
newtype ObjectInterrogativeMarker =
  ObjectInterrogativeMarker { _fromObjectInterrogativeMarker :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme ObjectInterrogativeMarker where
  toLexeme = _fromObjectInterrogativeMarker

objectInterrogativeMarker :: HashSet ObjectInterrogativeMarker
objectInterrogativeMarker = singleton $ ObjectInterrogativeMarker WHAT

type TraversalMarker :: Type
newtype TraversalMarker = TraversalMarker { _fromTraversalMarker :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme TraversalMarker where
  toLexeme = _fromTraversalMarker

traversalMarkers :: HashSet TraversalMarker
traversalMarkers = singleton $ TraversalMarker THROUGH

type ProcessingMethod :: Type
newtype ProcessingMethod = ProcessingMethod { _fromProcessingMethod :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme ProcessingMethod where
  toLexeme = _fromProcessingMethod

processingMethods :: HashSet ProcessingMethod
processingMethods = singleton $ ProcessingMethod THROUGH

type ContainmentMarker :: Type
newtype ContainmentMarker = ContainmentMarker { _fromContainmentMarker :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme ContainmentMarker where
  toLexeme = _fromContainmentMarker

containmentMarkers :: HashSet ContainmentMarker
containmentMarkers = fromList $ map ContainmentMarker [IN, INTO]

type SurfaceMarker :: Type
newtype SurfaceMarker = SurfaceMarker { _fromSurfaceMarker :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme SurfaceMarker where
  toLexeme = _fromSurfaceMarker

surfaceMarkers :: HashSet SurfaceMarker
surfaceMarkers = fromList $ map SurfaceMarker [ON, ONTO]

type RecipientMarker :: Type
newtype RecipientMarker = RecipientMarker { _fromRecipientMarker :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme RecipientMarker where
  toLexeme = _fromRecipientMarker

recipientMarker :: HashSet RecipientMarker
recipientMarker = fromList $ RecipientMarker <$> [TO]

type SourceMarker :: Type
newtype SourceMarker = SourceMarker { _fromSourceMarker :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme SourceMarker where
  toLexeme = _fromSourceMarker

sourceMarker :: HashSet SourceMarker
sourceMarker = fromList $ SourceMarker <$> [FROM]

type InstrumentalMarker :: Type
newtype InstrumentalMarker =
  InstrumentalMarker { _fromInstrumentalMarker :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instrumentalMarker :: HashSet InstrumentalMarker
instrumentalMarker = Data.HashSet.singleton $ InstrumentalMarker WITH

type Path :: Type
newtype Path = Path { _fromPath :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme Path where
  toLexeme = _fromPath

-- Prepositions that can signify direction
paths :: HashSet Path
paths = fromList
  $ map Path [THROUGH, TO, INTO,ACROSS,OVER,UNDER,AROUND,ONTO, UP,DOWN,BETWEEN,OFF]

type DirectionalStimulusMarker :: Type
newtype DirectionalStimulusMarker = DirectionalStimulusMarker { _fromDirectionalStimulusMarker :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme DirectionalStimulusMarker where
  toLexeme = _fromDirectionalStimulusMarker

directionalStimulusMarkers :: HashSet DirectionalStimulusMarker
directionalStimulusMarkers = singleton $ DirectionalStimulusMarker AT


type TargetedStimulusMarker :: Type
newtype TargetedStimulusMarker = TargetedStimulusMarker { _fromTargetedStimulusMarker :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme TargetedStimulusMarker where
  toLexeme = _fromTargetedStimulusMarker

targetedStimulusMarker :: HashSet TargetedStimulusMarker
targetedStimulusMarker = singleton $ TargetedStimulusMarker TO
-- look on the table
type SurfaceStimulusMarker :: Type
newtype SurfaceStimulusMarker = SurfaceStimulusMarker { _fromSurfaceStimulusMarker :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord)

instance HasLexeme SurfaceStimulusMarker where
  toLexeme = _fromSurfaceStimulusMarker

surfaceStimulusMarker :: HashSet SurfaceStimulusMarker
surfaceStimulusMarker = singleton $ SurfaceStimulusMarker ON

-- look in the box
type ContainerStimulusMarker :: Type
newtype ContainerStimulusMarker = ContainerStimulusMarker { _fromContainerStimulusMarker :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme ContainerStimulusMarker where
  toLexeme = _fromContainerStimulusMarker

containerStimulusMarker :: HashSet ContainerStimulusMarker
containerStimulusMarker = singleton $ ContainerStimulusMarker IN

type RegionMarker :: Type
newtype RegionMarker = RegionMarker { _fromRegionMarker :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme RegionMarker where
  toLexeme = _fromRegionMarker

regionMarker :: HashSet RegionMarker
regionMarker = fromList $ map RegionMarker [IN, ON]

type TReferentialMarker :: Type
newtype TReferentialMarker =
  TReferentialMarker { _fromTReferentialMarker :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme TReferentialMarker where
  toLexeme = _fromTReferentialMarker

-- Nouns that can be asked about Referential :: Topic
treferentialMarker :: HashSet TReferentialMarker
treferentialMarker = singleton $ TReferentialMarker UP

type TopicMarker :: Type
newtype TopicMarker = TopicMarker { _fromTopicMarker :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme TopicMarker where
  toLexeme = _fromTopicMarker

topicMarker :: HashSet TopicMarker
topicMarker = singleton $ TopicMarker ABOUT

type AxialMarker :: Type
newtype AxialMarker = AxialMarker { _fromAxialMarker :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme AxialMarker where
  toLexeme = _fromAxialMarker

axialMarker :: HashSet AxialMarker
axialMarker = fromList
  $ map AxialMarker [BEHIND, UNDER, ABOVE , FRONT, LEFT, RIGHT]

type VerticalAxis :: Type
newtype VerticalAxis = VerticalAxis { _fromVerticalAxis :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

verticalAxis :: HashSet VerticalAxis
verticalAxis = fromList $ map VerticalAxis [UNDER, ABOVE]

type FrontBackAxis :: Type
newtype FrontBackAxis = FrontBackAxis { _fromFrontBackAxis :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

frontBackAxis :: HashSet FrontBackAxis
frontBackAxis = fromList $ map FrontBackAxis [BEHIND, FRONT]

type LeftRightAxis :: Type
newtype LeftRightAxis = LeftRightAxis { _fromLeftRightAxis :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

leftRightAxis :: HashSet LeftRightAxis
leftRightAxis = fromList $ map LeftRightAxis [LEFT, RIGHT]

type ReferenceSourceMarker :: Type
newtype ReferenceSourceMarker = ReferenceSourceMarker { _fromReferenceSourceMarker :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme ReferenceSourceMarker where
  toLexeme = _fromReferenceSourceMarker

referenceSourceMarkers :: HashSet ReferenceSourceMarker
referenceSourceMarkers = singleton $ ReferenceSourceMarker IN

#ifdef TESTING

instance Arbitrary LocationInterrogativeMarker where
  arbitrary = elements $ HS.toList locationInterrogativeMarker

instance Arbitrary ObjectInterrogativeMarker where
  arbitrary = elements $ HS.toList objectInterrogativeMarker

instance Arbitrary TraversalMarker where
  arbitrary = elements $ HS.toList traversalMarkers

instance Arbitrary ProcessingMethod where
  arbitrary = elements $ HS.toList processingMethods

instance Arbitrary ContainmentMarker where
  arbitrary = elements $ HS.toList containmentMarkers

instance Arbitrary SurfaceMarker where
  arbitrary = elements $ HS.toList surfaceMarkers

instance Arbitrary RecipientMarker where
  arbitrary = elements $ HS.toList recipientMarker

instance Arbitrary SourceMarker where
  arbitrary = elements $ HS.toList sourceMarker

instance Arbitrary InstrumentalMarker where
  arbitrary = elements $ HS.toList instrumentalMarker

instance Arbitrary Path where
  arbitrary = elements $ HS.toList paths

instance Arbitrary DirectionalStimulusMarker where
  arbitrary = elements $ HS.toList directionalStimulusMarkers

instance Arbitrary TargetedStimulusMarker where
  arbitrary = elements $ HS.toList targetedStimulusMarker

instance Arbitrary SurfaceStimulusMarker where
  arbitrary = elements $ HS.toList surfaceStimulusMarker

instance Arbitrary ContainerStimulusMarker where
  arbitrary = elements $ HS.toList containerStimulusMarker

instance Arbitrary RegionMarker where
  arbitrary = elements $ HS.toList regionMarker

instance Arbitrary TReferentialMarker where
  arbitrary = elements $ HS.toList treferentialMarker

instance Arbitrary TopicMarker where
  arbitrary = elements $ HS.toList topicMarker

instance Arbitrary AxialMarker where
  arbitrary = elements $ HS.toList axialMarker

instance Arbitrary VerticalAxis where
  arbitrary = elements $ HS.toList verticalAxis

instance Arbitrary FrontBackAxis where
  arbitrary = elements $ HS.toList frontBackAxis

instance Arbitrary LeftRightAxis where
  arbitrary = elements $ HS.toList leftRightAxis

instance Arbitrary ReferenceSourceMarker where
  arbitrary = elements $ HS.toList referenceSourceMarkers

#endif
