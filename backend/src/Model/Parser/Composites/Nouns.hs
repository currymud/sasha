{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Model.Parser.Composites.Nouns (ContainerPhrase (SimpleContainerPhrase, ContainerPhrase ),
                                      ContainerPhraseRules (ContainerPhraseRules, _containerRule, _containerMarkerRule),
                                      DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                      DirectionalStimulusNounRules (DirectionalStimulusNounRules, _directionalStimulusRule),
                                      EdibleNounPhrase (EdibleNounPhrase),
                                      EdibleNounPhraseRules (EdibleNounPhraseRules, _edibleNounPhraseRule),
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
import           Data.Kind                         (Type)
import           Data.Text                         (Text, unwords)
import           GHC.Generics                      (Generic)
import           Model.Parser.Atomics.Adjectives   (Adjective)
import           Model.Parser.Atomics.Misc         (Determiner)
import           Model.Parser.Atomics.Nouns        (Container,
                                                    DirectionalStimulus, Edible,
                                                    Objective, SomaticStimulus,
                                                    Surface)
import           Model.Parser.Atomics.Prepositions (ContainmentMarker,
                                                    SurfaceMarker)
import           Model.Parser.Lexer                (Lexeme)
import           Prelude                           hiding (unwords)
import           Relude.String.Conversion          (ToText (toText))
import           Text.Earley                       (Prod)

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

type DirectionalStimulusNounPhrase :: Type
newtype DirectionalStimulusNounPhrase = DirectionalStimulusNounPhrase (NounPhrase DirectionalStimulus)
  deriving stock (Show, Eq, Ord,Generic)
  deriving newtype (ToText)

type DirectionalStimulusNounRules :: (Type -> Type -> Type -> Type) -> Type
newtype DirectionalStimulusNounRules r = DirectionalStimulusNounRules
  { _directionalStimulusRule :: Prod r Text Lexeme (NounPhrase DirectionalStimulus)}

type EdibleNounPhrase :: Type
newtype EdibleNounPhrase = EdibleNounPhrase (NounPhrase Edible)
  deriving stock (Show, Eq, Ord,Generic)
  deriving newtype (ToText)

type EdibleNounPhraseRules :: (Type -> Type -> Type -> Type) -> Type
newtype EdibleNounPhraseRules r = EdibleNounPhraseRules
  { _edibleNounPhraseRule :: Prod r Text Lexeme (NounPhrase Edible) }

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
  , _surfaceMarkerRule :: Prod r Text Lexeme (NounPhrase SurfaceMarker)
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
