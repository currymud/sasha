{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Model.Parser.Composites.Nouns (ContainerPhrase (ContainerPhrase ),
                                      ContainerPhraseRules (ContainerPhraseRules, _containerRule),
                                      DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                      DirectionalStimulusNounRules (DirectionalStimulusNounRules, _directionalStimulusRule),
                                      ConsumableNounPhrase (ConsumableNounPhrase),
                                      ConsumableNounPhraseRules (ConsumableNounPhraseRules, _consumableNounPhraseRule),
                                      InstrumentalAccessNounPhrase (InstrumentalAccessNounPhrase),
                                      NounPhrase (SimpleNounPhrase,
                                                  NounPhrase,
                                                  DescriptiveNounPhrase,
                                                  DescriptiveNounPhraseDet),
                                      NounPhraseRules (NounPhraseRules, _determinerRule, _adjRule, _nounRule),
                                      ObjectPhrase (ObjectPhrase),
                                      ObjectPhraseRules (ObjectPhraseRules, _objectiveRule),
                                      SomaticStimulusNounPhrase (SomaticStimulusNounPhrase),
                                      SomaticStimulusNounRules (SomaticStimulusNounRules,_somaticStimulusRule ),
                                      SupportPhraseRules (SupportPhraseRules,_surfacePhraseRule,_containerPhraseRule),
                                      SupportPhrase (SurfaceSupport, ContainerSupport),
                                      SurfacePhrase (SimpleSurfacePhrase, SurfacePhrase),
                                      SurfacePhraseRules (SurfacePhraseRules,_surfaceRule,_surfaceMarkerRule)) where
import           Data.Kind                            (Type)
import           Data.Text                            (Text, unwords)
import           GHC.Generics                         (Generic)
import           Model.Parser.Atomics.Adjectives      (Adjective)
import           Model.Parser.Atomics.Misc            (Determiner)
import           Model.Parser.Atomics.Nouns           (Consumable, Container,
                                                       DirectionalStimulus,
                                                       InstrumentalAccessNoun,
                                                       Objective,
                                                       SomaticStimulus, Surface)
import           Model.Parser.Atomics.Prepositions    (ContainmentMarker,
                                                       DirectionalStimulusMarker,
                                                       InstrumentMarker,
                                                       SurfaceMarker)
import           Model.Parser.Lexer                   (Lexeme)
import           Prelude                              hiding (unwords)
import           Relude.String.Conversion             (ToText (toText))
import           Text.Earley                          (Prod)
#ifdef TESTING
import           Grammar.Parser.Partitions.Adjectives ()
import           Grammar.Parser.Partitions.Misc       ()
import           Test.QuickCheck                      (Arbitrary (arbitrary),
                                                       oneof)
#endif

type ContainerPhrase :: Type
newtype ContainerPhrase = ContainerPhrase (NounPhrase Container)
  deriving stock (Show, Eq, Ord,Generic)

instance ToText ContainerPhrase where
  toText (ContainerPhrase nounPhrase) = toText nounPhrase

type ContainerPhraseRules :: (Type -> Type -> Type -> Type) -> Type
newtype ContainerPhraseRules r = ContainerPhraseRules { _containerRule :: Prod r Text Lexeme (NounPhrase Container)}

type InstrumentalAccessNounPhrase :: Type
data InstrumentalAccessNounPhrase = InstrumentalAccessNounPhrase InstrumentMarker (NounPhrase InstrumentalAccessNoun)
  deriving stock (Show, Eq, Ord,Generic)

instance ToText InstrumentalAccessNounPhrase where
  toText (InstrumentalAccessNounPhrase marker nounPhrase) =
    unwords [toText marker, toText nounPhrase]

type InstrumentalNounPhraseRules :: (Type -> Type -> Type -> Type) -> Type
data InstrumentalNounPhraseRules r = InstrumentalNounPhraseRules
  { _instrumentMarkerRule :: Prod r Text Lexeme InstrumentMarker
  , _instrumentNounRule   :: Prod r Text Lexeme (NounPhrase Objective)
  }

type DirectionalStimulusNounPhrase :: Type
data DirectionalStimulusNounPhrase =
  DirectionalStimulusNounPhrase DirectionalStimulusMarker (NounPhrase DirectionalStimulus)
  deriving stock (Show, Eq, Ord,Generic)

instance ToText DirectionalStimulusNounPhrase where
  toText (DirectionalStimulusNounPhrase marker nounPhrase) =
    unwords [toText marker, toText nounPhrase]

type DirectionalStimulusNounRules :: (Type -> Type -> Type -> Type) -> Type
data DirectionalStimulusNounRules r = DirectionalStimulusNounRules
  { _directionalStimulusRule :: Prod r Text Lexeme (NounPhrase DirectionalStimulus)
  , _directionalStimulusMarkerRule :: Prod r Text Lexeme DirectionalStimulusMarker
  }

type ConsumableNounPhrase :: Type
newtype ConsumableNounPhrase = ConsumableNounPhrase (NounPhrase Consumable)
  deriving stock (Show, Eq, Ord,Generic)
  deriving newtype (ToText)

type ConsumableNounPhraseRules :: (Type -> Type -> Type -> Type) -> Type
newtype ConsumableNounPhraseRules r = ConsumableNounPhraseRules
  { _consumableNounPhraseRule :: Prod r Text Lexeme (NounPhrase Consumable) }

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

type SomaticStimulusNounPhrase :: Type
newtype SomaticStimulusNounPhrase = SomaticStimulusNounPhrase (NounPhrase SomaticStimulus)
  deriving stock (Show, Eq, Ord,Generic)
  deriving newtype (ToText)

type SomaticStimulusNounRules :: (Type -> Type -> Type -> Type) -> Type
newtype SomaticStimulusNounRules r = SomaticStimulusNounRules
  { _somaticStimulusRule :: Prod r Text Lexeme (NounPhrase SomaticStimulus) }

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
  , _surfaceMarkerRule :: Prod r Text Lexeme SurfaceMarker
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

#ifdef TESTING
-- Arbitrary instance for NounPhrase (parameterized type)
instance Arbitrary a => Arbitrary (NounPhrase a) where
  arbitrary = oneof
    [ SimpleNounPhrase <$> arbitrary
    , NounPhrase <$> arbitrary <*> arbitrary
    , DescriptiveNounPhrase <$> arbitrary <*> arbitrary
    , DescriptiveNounPhraseDet <$> arbitrary <*> arbitrary <*> arbitrary
    ]
#endif
