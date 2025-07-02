module Model.Parser.Composites.Nouns where

import           Control.Applicative               (Alternative ((<|>)))
import           Data.Kind                         (Type)
import           Data.Text                         (Text, unwords)
import           GHC.Generics                      (Generic)
import           Lexer                             (Lexeme (..))
import           Model.Parser.Atomics.Adjectives   (Adjective)
import           Model.Parser.Atomics.Adverbs      (ImplicitPath,
                                                    ModToggleAdverb)
import           Model.Parser.Atomics.Misc         (Determiner)
import           Model.Parser.Atomics.Nouns        (Container,
                                                    DirectionalStimulus,
                                                    ModToggleNoun, ObjectPath,
                                                    Objective, SimpleAccessNoun,
                                                    Surface, TargetedStimulus,
                                                    ToggleNoun)
import           Model.Parser.Atomics.Prepositions (ContainmentMarker,
                                                    InstrumentalMarker, Path,
                                                    SurfaceMarker,
                                                    TargetedStimulusMarker)
import           Prelude                           hiding (unwords)
import           Relude.String.Conversion          (ToText, toText)
import           Text.Earley                       (Grammar)
import           Text.Earley.Grammar               (Prod, rule)
#ifdef TESTING
import           GHC.Generics                      (Generic)
import           Test.QuickCheck                   (Arbitrary (arbitrary, shrink),
                                                    arbitraryBoundedEnum, oneof)
import           Test.QuickCheck.Arbitrary.Generic (GenericArbitrary (..))
import           Test.QuickCheck.Instances.Text    ()
#endif

-- (runStateT . runExceptT) (runReaderT start config) defaultGameState
-- Plant the pot plant in the plant pot with the trowel
-- unlock the cabinet below the shelf

type ObjectPathRules :: (Type -> Type -> Type -> Type) -> Type
newtype ObjectPathRules r = ObjectPathPhraseRules
  {_objectPathRules :: Prod r Text Lexeme ObjectPath}

type ObjectPathPhrase :: Type
data ObjectPathPhrase
  = SimpleObjectPathPhrase ObjectPath
  | ObjectPathPhrase Determiner ObjectPath
  | ObjectPathPhraseAdj Determiner Adjective ObjectPath
  deriving stock (Show, Eq, Ord, Generic)

instance ToText ObjectPathPhrase where
  toText (SimpleObjectPathPhrase path) = toText path
  toText (ObjectPathPhrase det path) = unwords [toText det,toText path]
  toText (ObjectPathPhraseAdj det adj path) =
    unwords [toText det, toText adj, toText path]

type PathPhrase :: Type
data PathPhrase
  = SimplePath ImplicitPath
  | PathPhrase Path Determiner ObjectPath
  deriving stock (Show,Eq,Ord,Generic)

type PathPhraseRules :: (Type -> Type -> Type -> Type) -> Type
data PathPhraseRules r = PathPhraseRules
  { _pathRule         :: Prod r Text Lexeme Path
  , _implicitPathRule :: Prod r Text Lexeme ImplicitPath
  , _objectPathRule   :: Prod r Text Lexeme ObjectPath
  }

type PrepObjectPhrase :: Type
data PrepObjectPhrase
  = Instrument InstrumentalMarker ObjectPathPhrase
  deriving stock (Show, Eq, Ord,Generic)

type PrepObjectPhraseRules :: (Type -> Type -> Type -> Type) -> Type
data PrepObjectPhraseRules r = PrepObjectPhraseRules
  { _instrumentalMarkerRule :: Prod r Text Lexeme InstrumentalMarker
  , _objectPathPhraseRule   :: Prod r Text Lexeme ObjectPathPhrase
  }

type NounPhrase :: Type -> Type
data NounPhrase a
  = SimpleNounPhrase a
  | NounPhrase Determiner a
  | DescriptiveNounPhrase Adjective a
  | DescriptiveNounPhraseDet Determiner Adjective a
  deriving stock (Show, Eq, Ord,Generic)

instance ToText a => ToText (NounPhrase a) where
  toText (SimpleNounPhrase a) = toText a
  toText (NounPhrase det a) = unwords [toText det, toText a]
  toText (DescriptiveNounPhrase adj a) =
    unwords [toText adj, toText a]
  toText (DescriptiveNounPhraseDet det adj a) =
    unwords [toText det, toText adj, toText a]

type NounPhraseRules :: Type -> (Type -> Type -> Type -> Type) -> Type
data NounPhraseRules a r = NounPhraseRules
  { _determinerRule :: Prod r Text Lexeme Determiner
  , _adjRule        :: Prod r Text Lexeme Adjective
  , _nounRule       :: Prod r Text Lexeme a
  }

type ObjectPhrase :: Type
newtype ObjectPhrase = ObjectPhrase (NounPhrase Objective)
  deriving stock (Show, Eq, Ord,Generic)
  deriving newtype (ToText)

type ObjectPhraseRules :: (Type -> Type -> Type -> Type) -> Type
newtype ObjectPhraseRules r = ObjectPhraseRules { _objectiveRule :: Prod r Text Lexeme Objective }

type SurfacePhrase :: Type
data SurfacePhrase
  = SimpleSurfacePhrase (NounPhrase Surface)
  | SurfacePhrase SurfaceMarker (NounPhrase Surface)
  deriving stock (Show, Eq, Ord,Generic)

instance ToText SurfacePhrase where
  toText (SimpleSurfacePhrase nounPhrase) = toText nounPhrase
  toText (SurfacePhrase marker nounPhrase) =
    unwords [toText marker, toText nounPhrase]

type SurfacePhraseRules :: (Type -> Type -> Type -> Type) -> Type
data SurfacePhraseRules r = SurfacePhraseRules
  { _surfaceRule       :: Prod r Text Lexeme (NounPhrase Surface)
  , _surfaceMarkerRule :: Prod r Text Lexeme (NounPhrase SurfaceMarker)
  }

type ContainerPhrase :: Type
data ContainerPhrase
  = SimpleContainerPhrase (NounPhrase Container)
  | ContainerPhrase ContainmentMarker (NounPhrase Container)
  deriving stock (Show, Eq, Ord,Generic)

instance ToText ContainerPhrase where
  toText (SimpleContainerPhrase nounPhrase) = toText nounPhrase
  toText (ContainerPhrase marker nounPhrase) =
    unwords [toText marker, toText nounPhrase]

type ContainerPhraseRules :: (Type -> Type -> Type -> Type) -> Type
data ContainerPhraseRules r = ContainerPhraseRules
  { _containerRule       :: Prod r Text Lexeme (NounPhrase Container)
  , _containerMarkerRule :: Prod r Text Lexeme ContainmentMarker
  }

type SupportPhrase :: Type
data SupportPhrase
  = SurfaceSupport SurfacePhrase
  | ContainerSupport ContainerPhrase
  deriving stock (Show, Eq, Ord,Generic)

instance ToText SupportPhrase where
  toText (SurfaceSupport surfacePhrase)     = toText surfacePhrase
  toText (ContainerSupport containerPhrase) = toText containerPhrase

type SupportPhraseRules :: (Type -> Type -> Type -> Type) -> Type
data SupportPhraseRules r = SupportPhraseRules
  { _surfacePhraseRule   :: Prod r Text Lexeme SurfacePhrase
  , _containerPhraseRule :: Prod r Text Lexeme ContainerPhrase
  }

type DirectionalStimulusNounPhrase :: Type
newtype DirectionalStimulusNounPhrase = DirectionalStimulusNounPhrase (NounPhrase DirectionalStimulus)
  deriving stock (Show, Eq, Ord,Generic)
  deriving newtype (ToText)

type DirectionalStimulusNounRules :: (Type -> Type -> Type -> Type) -> Type
newtype DirectionalStimulusNounRules r = DirectionalStimulusNounRules
  { _directionalStimulusRule :: Prod r Text Lexeme DirectionalStimulus}

type ToggleNounPhrase :: Type
newtype ToggleNounPhrase = ToggleNounPhrase (NounPhrase ToggleNoun)
  deriving stock (Show, Eq, Ord,Generic)
  deriving newtype (ToText)

type ToggleNounPhraseRules :: (Type -> Type -> Type -> Type) -> Type
newtype ToggleNounPhraseRules r = ToggleNounPhraseRules
  { _toggleRule     :: Prod r Text Lexeme ToggleNoun}

type ModToggleNounPhrase :: Type
data ModToggleNounPhrase = ModToggleNounPhrase (NounPhrase ModToggleNoun) ModToggleAdverb
  deriving stock (Show, Eq, Ord,Generic)

instance ToText ModToggleNounPhrase where
  toText (ModToggleNounPhrase nounPhrase modToggleAdv) =
    unwords [toText nounPhrase, toText modToggleAdv]

type ModToggleNounPhraseRules :: (Type -> Type -> Type -> Type) -> Type
data ModToggleNounPhraseRules r = ModToggleNounPhraseRules
  { _modToggleNounRule :: Prod r Text Lexeme ModToggleNoun
  , _modToggleAdvRule  :: Prod r Text Lexeme ModToggleAdverb
  }

type TargetedStimulusNounPhrase :: Type
data TargetedStimulusNounPhrase
  = SimpleAgentStimulus TargetedStimulus
  | TargetedStimuliiNounPhrase TargetedStimulusMarker (NounPhrase TargetedStimulus)
  deriving stock (Show, Eq, Ord,Generic)

instance ToText TargetedStimulusNounPhrase where
  toText (SimpleAgentStimulus stimulus) = toText stimulus
  toText (TargetedStimuliiNounPhrase marker nounPhrase) =
    unwords [toText marker, toText nounPhrase]

type TargetedStimulusNounPhraseRules :: (Type -> Type -> Type -> Type) -> Type
data TargetedStimulusNounPhraseRules r = TargetedStimulusNounPhraseRules
  { _targetedStimulusMarkerRule :: Prod r Text Lexeme TargetedStimulusMarker
  , _targetedStimulusRule       :: Prod r Text Lexeme TargetedStimulus
  }

type SimpleAccessNounPhrase :: Type
newtype SimpleAccessNounPhrase = SimpleAccessNounPhrase (NounPhrase SimpleAccessNoun)
  deriving stock (Show, Eq, Ord,Generic)
  deriving newtype (ToText)

type SimpleAccessNounPhraseRules :: (Type -> Type -> Type -> Type) -> Type
newtype SimpleAccessNounPhraseRules r = SimpleAccessNounPhraseRules
  { _simpleAccessNounRule :: Prod r Text Lexeme SimpleAccessNoun }

#ifdef TESTING
deriving via GenericArbitrary PathPhrase
          instance Arbitrary PathPhrase
deriving via GenericArbitrary PrepObjectPhrase
          instance Arbitrary PrepObjectPhrase
instance Arbitrary a => Arbitrary (NounPhrase a) where
  arbitrary = oneof
    [ SimpleNounPhrase <$> arbitrary
    , NounPhrase <$> arbitrary <*> arbitrary
    , DescriptiveNounPhrase <$> arbitrary <*> arbitrary
    , DescriptiveNounPhraseDet <$> arbitrary <*> arbitrary <*> arbitrary
    ]

  shrink (SimpleNounPhrase a) =
    SimpleNounPhrase <$> shrink a
  shrink (NounPhrase det a) =
    [SimpleNounPhrase a] ++
    [NounPhrase det' a | det' <- shrink det] ++
    [NounPhrase det a' | a' <- shrink a]
  shrink (DescriptiveNounPhrase adj a) =
    [SimpleNounPhrase a] ++
    [DescriptiveNounPhrase adj' a | adj' <- shrink adj] ++
    [DescriptiveNounPhrase adj a' | a' <- shrink a]
  shrink (DescriptiveNounPhraseDet det adj a) =
    [SimpleNounPhrase a, NounPhrase det a, DescriptiveNounPhrase adj a] ++
    [DescriptiveNounPhraseDet det' adj a | det' <- shrink det] ++
    [DescriptiveNounPhraseDet det adj' a | adj' <- shrink adj] ++
    [DescriptiveNounPhraseDet det adj a' | a' <- shrink a]
deriving via GenericArbitrary ObjectPhrase
         instance Arbitrary ObjectPhrase
deriving via GenericArbitrary ObjectPathPhrase
         instance Arbitrary ObjectPathPhrase
deriving via GenericArbitrary SurfacePhrase
         instance Arbitrary SurfacePhrase
deriving via GenericArbitrary ContainerPhrase
         instance Arbitrary ContainerPhrase
deriving via GenericArbitrary SupportPhrase
         instance Arbitrary SupportPhrase
deriving via GenericArbitrary DirectionalStimulusNounPhrase
         instance Arbitrary DirectionalStimulusNounPhrase
deriving via GenericArbitrary ToggleNounPhrase
         instance Arbitrary ToggleNounPhrase
deriving via GenericArbitrary ModToggleNounPhrase
         instance Arbitrary ModToggleNounPhrase
deriving via GenericArbitrary TargetedStimulusNounPhrase
         instance Arbitrary TargetedStimulusNounPhrase
deriving via GenericArbitrary SimpleAccessNounPhrase
         instance Arbitrary SimpleAccessNounPhrase
#endif
