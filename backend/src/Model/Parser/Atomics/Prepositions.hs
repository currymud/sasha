module Model.Parser.Atomics.Prepositions where

import           Data.Hashable             (Hashable)
import           Data.HashSet              (HashSet, fromList, singleton,
                                            toList)
import           Data.Kind                 (Type)
import           GHC.Generics              (Generic)
import           Lexer
import           Relude.String.Conversion  (ToText)

#ifdef TESTING
import qualified Data.HashSet              as HS
import           Relude.String.Conversion  (ToText (toText))
import           Test.QuickCheck           (Arbitrary, elements)
import           Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
#endif

type LocationInterrogativeMarker :: Type
newtype LocationInterrogativeMarker =
  LocationInterrogativeMarker { _fromLocationInterrogativeMarker :: Lexeme }
  deriving stock (Show,Eq,Ord, Generic)
  deriving newtype (Hashable,ToText)

instance HasLexeme LocationInterrogativeMarker where
  toLexeme = _fromLocationInterrogativeMarker

-- For "what about" constructions
type ObjectInterrogativeMarker :: Type
newtype ObjectInterrogativeMarker =
  ObjectInterrogativeMarker { _fromObjectInterrogativeMarker :: Lexeme }
  deriving stock (Show,Eq,Ord, Generic)
  deriving newtype (Hashable,ToText)

instance HasLexeme ObjectInterrogativeMarker where
  toLexeme = _fromObjectInterrogativeMarker

type TraversalMarker :: Type
newtype TraversalMarker = TraversalMarker { _fromTraversalMarker :: Lexeme }
  deriving stock (Show,Eq,Ord, Generic)
  deriving newtype (Hashable,ToText)

instance HasLexeme TraversalMarker where
  toLexeme = _fromTraversalMarker

type ProcessingMethod :: Type
newtype ProcessingMethod = ProcessingMethod { _fromProcessingMethod :: Lexeme }
  deriving stock (Show,Eq,Ord,Generic)
  deriving newtype (Hashable,ToText)

instance HasLexeme ProcessingMethod where
  toLexeme = _fromProcessingMethod

type ContainmentMarker :: Type
newtype ContainmentMarker = ContainmentMarker { _fromContainmentMarker :: Lexeme }
  deriving stock (Show,Eq,Ord,Generic)
  deriving newtype (Hashable,ToText)

instance HasLexeme ContainmentMarker where
  toLexeme = _fromContainmentMarker

type SurfaceMarker :: Type
newtype SurfaceMarker = SurfaceMarker { _fromSurfaceMarker :: Lexeme }
  deriving stock (Show,Eq,Ord,Generic)
  deriving newtype (Hashable,ToText)

instance HasLexeme SurfaceMarker where
  toLexeme = _fromSurfaceMarker

type RecipientMarker :: Type
newtype RecipientMarker = RecipientMarker { _fromRecipientMarker :: Lexeme }
  deriving stock (Show,Eq,Ord,Generic)
  deriving newtype (Hashable,ToText)

instance HasLexeme RecipientMarker where
  toLexeme = _fromRecipientMarker

type SourceMarker :: Type
newtype SourceMarker = SourceMarker { _fromSourceMarker :: Lexeme }
  deriving stock (Show,Eq,Ord,Generic)
  deriving newtype (Hashable,ToText)

instance HasLexeme SourceMarker where
  toLexeme = _fromSourceMarker

type InstrumentalMarker :: Type
newtype InstrumentalMarker =
  InstrumentalMarker { _fromInstrumentalMarker :: Lexeme }
  deriving stock (Show,Eq,Ord,Generic)
  deriving newtype (Hashable,ToText)

instance HasLexeme InstrumentalMarker where
  toLexeme = _fromInstrumentalMarker

type Path :: Type
newtype Path = Path { _fromPath :: Lexeme }
  deriving stock (Show,Eq,Ord,Generic)
  deriving newtype (Hashable,ToText)

instance HasLexeme Path where
  toLexeme = _fromPath

type DirectionalStimulusMarker :: Type
newtype DirectionalStimulusMarker
  = DirectionalStimulusMarker { _fromDirectionalStimulusMarker :: Lexeme }
  deriving stock (Show,Eq,Ord,Generic)
  deriving newtype (Hashable,ToText)

instance HasLexeme DirectionalStimulusMarker where
  toLexeme = _fromDirectionalStimulusMarker

type TargetedStimulusMarker :: Type
newtype TargetedStimulusMarker = TargetedStimulusMarker { _fromTargetedStimulusMarker :: Lexeme }
  deriving stock (Show,Eq,Ord,Generic)
  deriving newtype (Hashable,ToText)

instance HasLexeme TargetedStimulusMarker where
  toLexeme = _fromTargetedStimulusMarker

type SurfaceStimulusMarker :: Type
newtype SurfaceStimulusMarker = SurfaceStimulusMarker { _fromSurfaceStimulusMarker :: Lexeme }
  deriving stock (Show,Eq,Ord,Generic)
  deriving newtype (Hashable,ToText)

instance HasLexeme SurfaceStimulusMarker where
  toLexeme = _fromSurfaceStimulusMarker

-- look in the box
type ContainerStimulusMarker :: Type
newtype ContainerStimulusMarker = ContainerStimulusMarker { _fromContainerStimulusMarker :: Lexeme }
  deriving stock (Show,Eq,Ord,Generic)
  deriving newtype (Hashable,ToText)

instance HasLexeme ContainerStimulusMarker where
  toLexeme = _fromContainerStimulusMarker

type RegionMarker :: Type
newtype RegionMarker = RegionMarker { _fromRegionMarker :: Lexeme }
  deriving stock (Show,Eq,Ord,Generic)
  deriving newtype (Hashable,ToText)

instance HasLexeme RegionMarker where
  toLexeme = _fromRegionMarker

type TReferentialMarker :: Type
newtype TReferentialMarker =
  TReferentialMarker { _fromTReferentialMarker :: Lexeme }
  deriving stock (Show,Eq,Ord,Generic)
  deriving newtype (Hashable,ToText)

instance HasLexeme TReferentialMarker where
  toLexeme = _fromTReferentialMarker

type TopicMarker :: Type
newtype TopicMarker = TopicMarker { _fromTopicMarker :: Lexeme }
  deriving stock (Show,Eq,Ord,Generic)
  deriving newtype (Hashable,ToText)

instance HasLexeme TopicMarker where
  toLexeme = _fromTopicMarker

type AxialMarker :: Type
newtype AxialMarker = AxialMarker { _fromAxialMarker :: Lexeme }
  deriving stock (Show,Eq,Ord,Generic)
  deriving newtype (Hashable,ToText)

instance HasLexeme AxialMarker where
  toLexeme = _fromAxialMarker

type VerticalAxis :: Type
newtype VerticalAxis = VerticalAxis { _fromVerticalAxis :: Lexeme }
  deriving stock (Show,Eq,Ord,Generic)
  deriving newtype (Hashable,ToText)

type FrontBackAxis :: Type
newtype FrontBackAxis = FrontBackAxis { _fromFrontBackAxis :: Lexeme }
  deriving stock (Show,Eq,Ord,Generic)
  deriving newtype (Hashable,ToText)

type LeftRightAxis :: Type
newtype LeftRightAxis = LeftRightAxis { _fromLeftRightAxis :: Lexeme }
  deriving stock (Show,Eq,Ord,Generic)
  deriving newtype (Hashable,ToText)

type ReferenceSourceMarker :: Type
newtype ReferenceSourceMarker = ReferenceSourceMarker { _fromReferenceSourceMarker :: Lexeme }
  deriving stock (Show,Eq,Ord,Generic)
  deriving newtype (Hashable,ToText)

instance HasLexeme ReferenceSourceMarker where
  toLexeme = _fromReferenceSourceMarker

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
