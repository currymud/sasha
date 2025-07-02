module Model.Parser.Composites.Prepositions where

import           Control.Applicative               (Alternative ((<|>)))
import           Data.Kind                         (Type)
import           Data.Text                         (Text, unwords)
import           GHC.Generics                      (Generic)
import           Lexer                             (Lexeme (..))
import           Parser.Rules.Atomics.Nouns        (NamedAgent)
import           Parser.Rules.Atomics.Prepositions (ContainmentMarker,
                                                    InstrumentalMarker, Path,
                                                    RecipientMarker,
                                                    TargetedStimulusMarker,
                                                    TraversalMarker)
import           Parser.Rules.Composites.Nouns     (ContainerPhrase,
                                                    ObjectPathPhrase,
                                                    ObjectPhrase,
                                                    TargetedStimulusNounPhrase)
import           Prelude                           hiding (Generic, unwords)
import           Relude.String.Conversion          (ToText (toText))
import           Text.Earley                       (Grammar, rule)
import           Text.Earley.Grammar               (Prod)
#ifdef TESTING
import           GHC.Generics                      (Generic)
import           Test.QuickCheck                   (Arbitrary (arbitrary),
                                                    arbitraryBoundedEnum)
import           Test.QuickCheck.Arbitrary.Generic (GenericArbitrary (..))
import           Test.QuickCheck.Instances.Text    ()
#endif

-- (runStateT . runExceptT) (runReaderT start config) defaultGameState
-- Plant the pot plant in the plant pot with the trowel
-- unlock the cabinet below the shelf

-- Do we even need these?
type InstrumentMarkerPhrase :: Type
data InstrumentMarkerPhrase
  = Instrument InstrumentalMarker ObjectPathPhrase
  deriving stock (Show, Eq, Ord, Generic)

instance ToText InstrumentMarkerPhrase where
  toText (Instrument marker path) = unwords [toText marker, toText path]

type InstrumentMarkerPhraseRules :: (Type -> Type -> Type -> Type) -> Type
data InstrumentMarkerPhraseRules r = InstrumentMarkerPhraseRules
  { _instrumentMarker :: Prod r Text Lexeme InstrumentalMarker
  , _objectPathPhrase :: Prod r Text Lexeme ObjectPathPhrase
  }

instrumentMarkerPhraseRules :: InstrumentMarkerPhraseRules r
                           -> Grammar r (Prod r Text Lexeme InstrumentMarkerPhrase)
instrumentMarkerPhraseRules (InstrumentMarkerPhraseRules {..}) =
  rule $ Instrument <$> _instrumentMarker <*> _objectPathPhrase

-- yes, we do

type TargetedMarkerPhrase :: Type
data TargetedMarkerPhrase
  = TargetedStimulusMarker TargetedStimulusMarker TargetedStimulusNounPhrase
  | RecipientMarker RecipientMarker NamedAgent
  deriving stock (Show, Eq, Ord, Generic)

instance ToText TargetedMarkerPhrase where
  toText (TargetedStimulusMarker marker noun) =
    unwords [toText marker, toText noun]
  toText (RecipientMarker marker agent) =
    unwords [toText marker, toText agent]

type TargetedMarkerPhraseRules :: (Type -> Type -> Type -> Type) -> Type
data TargetedMarkerPhraseRules r = TargetedMarkerPhraseRules
  { _targetedStimulusMarker     :: Prod r Text Lexeme TargetedStimulusMarker
  , _recipientMarker            :: Prod r Text Lexeme RecipientMarker
  , _targetedStimulusNounPhrase :: Prod r Text Lexeme TargetedStimulusNounPhrase
  , _namedAgent                 :: Prod r Text Lexeme NamedAgent
  }

targetedMarkerPhraseRules :: TargetedMarkerPhraseRules r
                          -> Grammar r (Prod r Text Lexeme TargetedMarkerPhrase)
targetedMarkerPhraseRules (TargetedMarkerPhraseRules {..}) =
  rule $ TargetedStimulusMarker <$> _targetedStimulusMarker <*> _targetedStimulusNounPhrase
       <|> RecipientMarker <$> _recipientMarker <*> _namedAgent

type TraversalPathPhrase :: Type
data TraversalPathPhrase
  = PathTraversal Path ObjectPhrase          -- for general paths like TO, INTO
  | DirectTraversal TraversalMarker ObjectPhrase  -- specifically for THROUGH
  | ContainmentTraversal ContainmentMarker ContainerPhrase  -- for IN, INTO as containers
  deriving stock (Show, Eq, Ord, Generic)

instance ToText TraversalPathPhrase where
  toText (PathTraversal path obj) =
    unwords [toText path, toText obj]
  toText (DirectTraversal marker obj) =
    unwords [toText marker, toText obj]
  toText (ContainmentTraversal marker cont) =
    unwords [toText marker, toText cont]

type TraversalPathPhraseRules :: (Type -> Type -> Type -> Type) -> Type
data TraversalPathPhraseRules r = TraversalPathPhraseRules
  { _path            :: Prod r Text Lexeme Path
  , _traversalMarker :: Prod r Text Lexeme TraversalMarker
  , _objectPhrase    :: Prod r Text Lexeme ObjectPhrase
  , _containerMarker :: Prod r Text Lexeme ContainmentMarker
  , _containerPhrase :: Prod r Text Lexeme ContainerPhrase
  }

traversalPathPhraseRule :: TraversalPathPhraseRules r
                            -> Grammar r (Prod r Text Lexeme TraversalPathPhrase)
traversalPathPhraseRule (TraversalPathPhraseRules {..}) =
  rule $ PathTraversal <$> _path <*> _objectPhrase
       <|> DirectTraversal <$> _traversalMarker <*> _objectPhrase
       <|> ContainmentTraversal <$> _containerMarker <*> _containerPhrase

#ifdef TESTING
deriving via GenericArbitrary InstrumentMarkerPhrase
  instance Arbitrary InstrumentMarkerPhrase
deriving via GenericArbitrary TargetedMarkerPhrase
  instance Arbitrary TargetedMarkerPhrase
deriving via GenericArbitrary TraversalPathPhrase
  instance Arbitrary TraversalPathPhrase
#endif
