module Parser.SpeechParts.Composites.Nouns where

import           Control.Applicative                      (Alternative ((<|>)))
import           Data.Kind                                (Type)
import           Data.Text                                (Text, unwords)
import           GHC.Generics                             (Generic)
import           Lexer                                    (Lexeme (..))
import           Parser.SpeechParts.Atomics.Adverbs       (ImplicitPath,
                                                           ModToggleAdverb)
import           Parser.SpeechParts.Atomics.Misc          (Determiner)
import           Parser.SpeechParts.Atomics.Nouns         (Container,
                                                           DirectionalStimulus,
                                                           ModToggleNoun,
                                                           ObjectPath,
                                                           Objective,
                                                           SimpleAccessNoun,
                                                           Surface,
                                                           TargetedStimulus,
                                                           ToggleNoun)
import           Parser.SpeechParts.Atomics.Prepositions  (ContainmentMarker,
                                                           InstrumentalMarker,
                                                           Path, SurfaceMarker,
                                                           TargetedStimulusMarker)
import           Parser.SpeechParts.Composites.Adjectives (AdjPhrase)
import           Prelude                                  hiding (unwords)
import           Relude.String.Conversion                 (ToText, toText)
import           Text.Earley                              (Grammar)
import           Text.Earley.Grammar                      (Prod, rule)
#ifdef TESTING
import           GHC.Generics                             (Generic)
import           Test.QuickCheck                          (Arbitrary (arbitrary, shrink),
                                                           arbitraryBoundedEnum,
                                                           oneof)
import           Test.QuickCheck.Arbitrary.Generic        (GenericArbitrary (..))
import           Test.QuickCheck.Instances.Text           ()
#endif

-- (runStateT . runExceptT) (runReaderT start config) defaultGameState
-- Plant the pot plant in the plant pot with the trowel
-- unlock the cabinet below the shelf

type ObjectPathPhraseRules :: (Type -> Type -> Type -> Type) -> Type
data ObjectPathPhraseRules r = ObjectPathPhraseRules
  { _objectPathRule :: Prod r Text Lexeme ObjectPath
  , _determinerRule :: Prod r Text Lexeme Determiner
  , _adjPhraseRule  :: Prod r Text Lexeme AdjPhrase
  }

type ObjectPathPhrase :: Type
data ObjectPathPhrase
  = SimpleObjectPathPhrase ObjectPath
  | ObjectPathPhrase  Determiner ObjectPath
  | ObjectPathPhraseAdj Determiner AdjPhrase ObjectPath
  deriving stock (Show, Eq, Ord, Generic)

instance ToText ObjectPathPhrase where
  toText (SimpleObjectPathPhrase path) = toText path
  toText (ObjectPathPhrase det path) = unwords [toText det,toText path]
  toText (ObjectPathPhraseAdj det adj path) =
    unwords [toText det, toText adj, toText path]

objectPathPhraseRule :: ObjectPathPhraseRules r
                          -> Grammar r (Prod r Text Lexeme ObjectPathPhrase)
objectPathPhraseRule (ObjectPathPhraseRules {..}) =
  rule $ SimpleObjectPathPhrase <$> _objectPathRule
           <|> ObjectPathPhrase <$> _determinerRule <*> _objectPathRule
           <|> ObjectPathPhraseAdj
                 <$> _determinerRule
                 <*> _adjPhraseRule
                 <*> _objectPathRule

type PathPhrase :: Type
data PathPhrase
  = SimplePath ImplicitPath
  | PathPhrase Path Determiner ObjectPath
  deriving stock (Show,Eq,Ord,Generic)

type PathPhraseRules :: (Type -> Type -> Type -> Type) -> Type
data PathPhraseRules r = PathPhraseRules
  { _pathRule         :: Prod r Text Lexeme Path
  , _implicitPathRule :: Prod r Text Lexeme ImplicitPath
  , _determinerRule   :: Prod r Text Lexeme Determiner
  , _objectPathRule   :: Prod r Text Lexeme ObjectPath
  }

pathPhraseRule :: PathPhraseRules r
                  -> Grammar r (Prod r Text Lexeme PathPhrase)
pathPhraseRule (PathPhraseRules {..}) =
  rule $ SimplePath <$> _implicitPathRule
           <|> PathPhrase <$> _pathRule <*> _determinerRule <*> _objectPathRule

type PrepObjectPhrase :: Type
data PrepObjectPhrase
  = Instrument InstrumentalMarker ObjectPathPhrase
  deriving stock (Show, Eq, Ord,Generic)

type PrepObjectPhraseRules :: (Type -> Type -> Type -> Type) -> Type
data PrepObjectPhraseRules r = PrepObjectPhraseRules
  { _instrumentalMarkerRule :: Prod r Text Lexeme InstrumentalMarker
  , _objectPathPhraseRule   :: Prod r Text Lexeme ObjectPathPhrase
  }

prepObjectPhraseRule :: PrepObjectPhraseRules r
                        -> Grammar r (Prod r Text Lexeme PrepObjectPhrase)
prepObjectPhraseRule (PrepObjectPhraseRules {..}) =
  rule $ Instrument <$> _instrumentalMarkerRule <*> _objectPathPhraseRule

type NounPhrase :: Type -> Type
data NounPhrase a
  = SimpleNounPhrase a
  | NounPhrase Determiner a
  | DescriptiveNounPhrase AdjPhrase a
  | DescriptiveNounPhraseDet Determiner AdjPhrase a
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
  , _adjPhraseRule  :: Prod r Text Lexeme AdjPhrase
  , _nounRule       :: Prod r Text Lexeme a
  }

nounPhraseRule :: NounPhraseRules a r
                    -> Grammar r (Prod r Text Lexeme (NounPhrase a))
nounPhraseRule (NounPhraseRules{..}) =
  rule $ SimpleNounPhrase <$> _nounRule
           <|> NounPhrase <$> _determinerRule <*> _nounRule
           <|> DescriptiveNounPhrase <$> _adjPhraseRule <*> _nounRule
           <|> DescriptiveNounPhraseDet
                 <$> _determinerRule
                 <*> _adjPhraseRule
                 <*> _nounRule

type ObjectPhrase :: Type
newtype ObjectPhrase = ObjectPhrase (NounPhrase Objective)
  deriving stock (Show, Eq, Ord,Generic)
  deriving newtype (ToText)

type ObjectPhraseRules :: (Type -> Type -> Type -> Type) -> Type
data ObjectPhraseRules r = ObjectPhraseRules
  { _determinerRule :: Prod r Text Lexeme Determiner
  , _objectiveRule  :: Prod r Text Lexeme Objective
  , _adjPhraseRule  :: Prod r Text Lexeme AdjPhrase
  }

objectPhraseRule :: ObjectPhraseRules r
                      -> Grammar r (Prod r Text Lexeme ObjectPhrase)
objectPhraseRule (ObjectPhraseRules {..}) =
  nounPhraseRule rules >>= \nounPhrase ->
    rule $ ObjectPhrase <$> nounPhrase
  where
   rules
      = NounPhraseRules
          { _determinerRule = _determinerRule
          , _adjPhraseRule = _adjPhraseRule
          , _nounRule = _objectiveRule
          }

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
  { _determinerRule    :: Prod r Text Lexeme Determiner
  , _adjPhraseRule     :: Prod r Text Lexeme AdjPhrase
  , _surfaceRule       :: Prod r Text Lexeme Surface
  , _surfaceMarkerRule :: Prod r Text Lexeme SurfaceMarker
  }

surfacePhraseRule :: SurfacePhraseRules r
                      -> Grammar r (Prod r Text Lexeme SurfacePhrase)
surfacePhraseRule (SurfacePhraseRules{..}) =
  nounPhraseRule rules >>= \nounPhrase ->
    rule $ SimpleSurfacePhrase <$> nounPhrase
             <|> SurfacePhrase <$> _surfaceMarkerRule <*> nounPhrase
  where
   rules
      = NounPhraseRules
          { _determinerRule = _determinerRule
          , _adjPhraseRule = _adjPhraseRule
          , _nounRule = _surfaceRule
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
  { _determinerRule      :: Prod r Text Lexeme Determiner
  , _adjPhraseRule       :: Prod r Text Lexeme AdjPhrase
  , _containerRule       :: Prod r Text Lexeme Container
  , _containerMarkerRule :: Prod r Text Lexeme ContainmentMarker
  }

containerPhraseRule :: ContainerPhraseRules r
                        -> Grammar r (Prod r Text Lexeme ContainerPhrase)
containerPhraseRule (ContainerPhraseRules{..}) =
  nounPhraseRule rules >>= \nounPhrase ->
    rule $ SimpleContainerPhrase <$> nounPhrase
             <|> ContainerPhrase <$> _containerMarkerRule <*> nounPhrase
  where
   rules
      = NounPhraseRules
          { _determinerRule = _determinerRule
          , _adjPhraseRule = _adjPhraseRule
          , _nounRule = _containerRule
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

supportPhraseRule :: SupportPhraseRules r
                     -> Grammar r (Prod r Text Lexeme SupportPhrase)
supportPhraseRule (SupportPhraseRules{..}) =
  rule $ SurfaceSupport <$> _surfacePhraseRule
           <|> ContainerSupport <$> _containerPhraseRule

type DirectionalStimulusNounPhrase :: Type
newtype DirectionalStimulusNounPhrase = DirectionalStimulusNounPhrase (NounPhrase DirectionalStimulus)
  deriving stock (Show, Eq, Ord,Generic)
  deriving newtype (ToText)

type DirectionalStimulusNounRules :: (Type -> Type -> Type -> Type) -> Type
data DirectionalStimulusNounRules r = DirectionalStimulusNounRules
  { _determinerRule          :: Prod r Text Lexeme Determiner
  , _adjPhraseRule           :: Prod r Text Lexeme AdjPhrase
  , _directionalStimulusRule :: Prod r Text Lexeme DirectionalStimulus
  }

directionalStimulusNounPhraseRule :: DirectionalStimulusNounRules r
                                -> Grammar r (Prod r Text Lexeme DirectionalStimulusNounPhrase)
directionalStimulusNounPhraseRule (DirectionalStimulusNounRules{..}) =
  nounPhraseRule rules >>= \nounPhrase ->
    rule $ DirectionalStimulusNounPhrase <$> nounPhrase
  where
   rules
      = NounPhraseRules
          { _determinerRule = _determinerRule
          , _adjPhraseRule = _adjPhraseRule
          , _nounRule = _directionalStimulusRule
          }

type ToggleNounPhrase :: Type
newtype ToggleNounPhrase = ToggleNounPhrase (NounPhrase ToggleNoun)
  deriving stock (Show, Eq, Ord,Generic)
  deriving newtype (ToText)

type ToggleNounPhraseRules :: (Type -> Type -> Type -> Type) -> Type
data ToggleNounPhraseRules r = ToggleNounPhraseRules
  { _determinerRule :: Prod r Text Lexeme Determiner
  , _adjPhraseRule  :: Prod r Text Lexeme AdjPhrase
  , _toggleRule     :: Prod r Text Lexeme ToggleNoun
  }

toggleNounPhraseRule :: ToggleNounPhraseRules r
                        -> Grammar r (Prod r Text Lexeme ToggleNounPhrase)
toggleNounPhraseRule (ToggleNounPhraseRules{..}) =
  nounPhraseRule rules >>= \nounPhrase ->
    rule $ ToggleNounPhrase <$> nounPhrase
  where
   rules
      = NounPhraseRules
          { _determinerRule = _determinerRule
          , _adjPhraseRule = _adjPhraseRule
          , _nounRule = _toggleRule
          }

type ModToggleNounPhrase :: Type
data ModToggleNounPhrase = ModToggleNounPhrase (NounPhrase ModToggleNoun) ModToggleAdverb
  deriving stock (Show, Eq, Ord,Generic)

instance ToText ModToggleNounPhrase where
  toText (ModToggleNounPhrase nounPhrase modToggleAdv) =
    unwords [toText nounPhrase, toText modToggleAdv]

type ModToggleNounPhraseRules :: (Type -> Type -> Type -> Type) -> Type
data ModToggleNounPhraseRules r = ModToggleNounPhraseRules
  { _determinerRule    :: Prod r Text Lexeme Determiner
  , _adjPhraseRule     :: Prod r Text Lexeme AdjPhrase
  , _modToggleNounRule :: Prod r Text Lexeme ModToggleNoun
  , _modToggleAdvRule  :: Prod r Text Lexeme ModToggleAdverb
  }

modToggleNounPhraseRule :: ModToggleNounPhraseRules r
                        -> Grammar r (Prod r Text Lexeme ModToggleNounPhrase)
modToggleNounPhraseRule (ModToggleNounPhraseRules{..}) =
  nounPhraseRule rules >>= \nounPhrase ->
    rule $ ModToggleNounPhrase <$> nounPhrase <*> _modToggleAdvRule
  where
   rules
      = NounPhraseRules
          { _determinerRule = _determinerRule
          , _adjPhraseRule = _adjPhraseRule
          , _nounRule = _modToggleNounRule
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
  , _determinerRule             :: Prod r Text Lexeme Determiner
  , _adjPhraseRule              :: Prod r Text Lexeme AdjPhrase
  }

targetedStimulusNounPhraseRule :: TargetedStimulusNounPhraseRules r
                                  -> Grammar r (Prod r Text Lexeme TargetedStimulusNounPhrase)
targetedStimulusNounPhraseRule (TargetedStimulusNounPhraseRules{..}) =
  nounPhraseRule rules >>= \nounPhrase ->
    rule $ SimpleAgentStimulus <$> _targetedStimulusRule
             <|> TargetedStimuliiNounPhrase <$> _targetedStimulusMarkerRule <*> nounPhrase
  where
   rules
      = NounPhraseRules
          { _determinerRule = _determinerRule
          , _adjPhraseRule = _adjPhraseRule
          , _nounRule = _targetedStimulusRule
          }

type SimpleAccessNounPhrase :: Type
newtype SimpleAccessNounPhrase = SimpleAccessNounPhrase (NounPhrase SimpleAccessNoun)
  deriving stock (Show, Eq, Ord,Generic)

type SimpleAccessNounPhraseRules :: (Type -> Type -> Type -> Type) -> Type
data SimpleAccessNounPhraseRules r = SimpleAccessNounPhraseRules
  { _determinerRule       :: Prod r Text Lexeme Determiner
  , _adjPhraseRule        :: Prod r Text Lexeme AdjPhrase
  , _simpleAccessNounRule :: Prod r Text Lexeme SimpleAccessNoun
  }

simpleAccessNounPhraseRule :: SimpleAccessNounPhraseRules r
                          -> Grammar r
                               (Prod r Text Lexeme SimpleAccessNounPhrase)
simpleAccessNounPhraseRule (SimpleAccessNounPhraseRules{..}) =
  nounPhraseRule rules >>= \nounPhrase ->
    rule $ SimpleAccessNounPhrase <$> nounPhrase
  where
   rules
      = NounPhraseRules
          { _determinerRule = _determinerRule
          , _adjPhraseRule = _adjPhraseRule
          , _nounRule = _simpleAccessNounRule
          }
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
