{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
module Parser.SpeechParts.Composites.Verbs where

import           Control.Applicative                        (Alternative ((<|>)))
import           Data.Kind                                  (Type)
import           Data.Text                                  (Text, unwords)
import           Debug.Trace                                (trace)
import           GHC.Generics                               (Generic)
import           Lexer                                      (Lexeme (..))
import           Parser.SpeechParts.Atomics.Adverbs         (ImplicitPath)
import           Parser.SpeechParts.Atomics.Misc            (Partition)
import           Parser.SpeechParts.Atomics.Nouns           (Agent, NamedAgent)
import           Parser.SpeechParts.Atomics.Prepositions    (DirectionalStimulusMarker,
                                                             LocationInterrogativeMarker,
                                                             ObjectInterrogativeMarker,
                                                             SourceMarker,
                                                             TopicMarker)
import           Parser.SpeechParts.Atomics.Verbs           (AcquisitionVerb,
                                                             CardinalMovementVerb,
                                                             Copula,
                                                             DirectionalStimulusVerb,
                                                             ExplicitStimulusVerb,
                                                             GeneralPlacementVerb,
                                                             ImplicitRegionalStimulusVerb,
                                                             ImplicitStimulusVerb,
                                                             ModToggleVerb,
                                                             SimpleAccessVerb,
                                                             TargetedStimulusVerb,
                                                             ToggleVerb,
                                                             TraversalVerb)
import           Parser.SpeechParts.Composites.Nouns        (ContainerPhrase,
                                                             DirectionalStimulusNounPhrase,
                                                             ModToggleNounPhrase,
                                                             ObjectPhrase,
                                                             SimpleAccessNounPhrase,
                                                             SupportPhrase,
                                                             TargetedStimulusNounPhrase,
                                                             ToggleNounPhrase)
import           Parser.SpeechParts.Composites.Prepositions (TraversalPathPhrase)
import           Prelude                                    hiding (unwords)
import           Relude.String                              (ToText (toText))
import           Relude.String.Conversion                   (ToText)
import           Text.Earley                                (Grammar)
import           Text.Earley.Grammar                        (Prod, rule)
#ifdef TESTING
import           GHC.Generics                               (Generic)
import           Parser.SpeechParts.Atomics.Misc            (Partition (Partition))
import           Test.QuickCheck                            (Arbitrary (arbitrary, shrink),
                                                             arbitraryBoundedEnum,
                                                             oneof)
import           Test.QuickCheck.Arbitrary.Generic          (GenericArbitrary (..))
import           Test.QuickCheck.Instances.Text             ()
#endif

-- (runStateT . runExceptT) (runReaderT start config) defaultGameState
-- Plant the pot plant in the plant pot with the trowel
-- unlock the cabinet below the shelf

type TraversalVerbPhrase :: Type
data TraversalVerbPhrase
  = SimpleTraversalVerbPhrase TraversalVerb ObjectPhrase
  | TraversalVerbPathPhrase TraversalVerb ObjectPhrase TraversalPathPhrase
  deriving stock (Show, Eq, Ord, Generic)

instance ToText TraversalVerbPhrase where
  toText (SimpleTraversalVerbPhrase verb obj) =
    unwords [toText verb, toText obj]
  toText (TraversalVerbPathPhrase verb obj path) =
    unwords [toText verb, toText obj, toText path]

type TraversalVerbPhraseRules :: (Type -> Type -> Type -> Type) -> Type
data TraversalVerbPhraseRules r = TraversalVerbPhraseRules
  { _traversalVerb       :: Prod r Text Lexeme TraversalVerb
  , _objectPhrase        :: Prod r Text Lexeme ObjectPhrase
  , _traversalPathPhrase :: Prod r Text Lexeme TraversalPathPhrase
  }

traversalVerbPhraseRule :: TraversalVerbPhraseRules r
                             -> Grammar r (Prod r Text Lexeme TraversalVerbPhrase)
traversalVerbPhraseRule (TraversalVerbPhraseRules {..}) =
  rule $ SimpleTraversalVerbPhrase <$> _traversalVerb <*> _objectPhrase
           <|> TraversalVerbPathPhrase
                 <$> _traversalVerb
                 <*> _objectPhrase
                 <*> _traversalPathPhrase

type GeneralPlacementVerbPhrase :: Type
data GeneralPlacementVerbPhrase
  = SimpleGeneralPlacementVerbPhrase GeneralPlacementVerb ObjectPhrase
  | GeneralPlacementVerbPhrase
      GeneralPlacementVerb
      ObjectPhrase
      SupportPhrase
  deriving stock (Show, Eq, Ord,Generic)

instance ToText GeneralPlacementVerbPhrase where
  toText (SimpleGeneralPlacementVerbPhrase verb obj) =
    unwords [toText verb, toText obj]
  toText (GeneralPlacementVerbPhrase verb obj support) =
    unwords [toText verb, toText obj, toText support]

type GeneralPlacementVerbPhraseRules :: (Type -> Type -> Type -> Type) -> Type
data GeneralPlacementVerbPhraseRules r = GeneralPlacementVerbPhraseRules
  { _generalPlacementVerb :: Prod r Text Lexeme GeneralPlacementVerb
  , _objectPhrase         :: Prod r Text Lexeme ObjectPhrase
  , _supportPhrase        :: Prod r Text Lexeme SupportPhrase
  }

type GeneralPlacementVerbPhraseGrammar :: (Type -> Type -> Type -> Type) -> Type
type GeneralPlacementVerbPhraseGrammar r
  = Grammar r (Prod r Text Lexeme GeneralPlacementVerbPhrase)

generalPlacementVerbPhraseRule :: GeneralPlacementVerbPhraseRules r
                                    -> Grammar r (Prod r Text Lexeme GeneralPlacementVerbPhrase)
generalPlacementVerbPhraseRule (GeneralPlacementVerbPhraseRules {..}) =
  rule $ SimpleGeneralPlacementVerbPhrase <$> _generalPlacementVerb <*> _objectPhrase
           <|> GeneralPlacementVerbPhrase
                 <$> _generalPlacementVerb
                 <*> _objectPhrase
                 <*> _supportPhrase

type AcquisitionVerbPhrase :: Type
data AcquisitionVerbPhrase
  = SimpleAcquisitionVerbPhrase AcquisitionVerb ObjectPhrase
  | AcquisitionVerbPhrase
      AcquisitionVerb
      ObjectPhrase
      SourceMarker
      SupportPhrase
  deriving stock (Show, Eq, Ord,Generic)

instance ToText AcquisitionVerbPhrase where
  toText (SimpleAcquisitionVerbPhrase verb obj) =
    unwords [toText verb, toText obj]
  toText (AcquisitionVerbPhrase verb obj source support) =
    unwords [toText verb, toText obj, toText source, toText support]
type AcquisitionVerbPhraseRules :: (Type -> Type -> Type -> Type) -> Type
data AcquisitionVerbPhraseRules r = AcquisitionVerbPhraseRules
  { _acquisitionVerb :: Prod r Text Lexeme AcquisitionVerb
  , _objectPhrase    :: Prod r Text Lexeme ObjectPhrase
  , _sourceMarker    :: Prod r Text Lexeme SourceMarker
  , _supporter       :: Prod r Text Lexeme SupportPhrase
  }

type AcquisitionVerbPhraseGrammar :: (Type -> Type -> Type -> Type) -> Type
type AcquisitionVerbPhraseGrammar r
  = Grammar r (Prod r Text Lexeme AcquisitionVerbPhrase)

acquisitionVerbPhraseRule :: AcquisitionVerbPhraseRules r
                             -> AcquisitionVerbPhraseGrammar r
acquisitionVerbPhraseRule (AcquisitionVerbPhraseRules {..}) =
  rule $ SimpleAcquisitionVerbPhrase <$> _acquisitionVerb <*> _objectPhrase
           <|> AcquisitionVerbPhrase
                 <$> _acquisitionVerb
                 <*> _objectPhrase
                 <*> _sourceMarker
                 <*> _supporter

type AccessVerbPhrase :: Type
data AccessVerbPhrase
  = ToggleVerbPhrase ToggleVerb ToggleNounPhrase
  | ModToggleVerbPhrase ModToggleVerb ModToggleNounPhrase
  | SimpleAccessVerbPhrase SimpleAccessVerb SimpleAccessNounPhrase
  deriving stock (Show, Eq, Ord, Generic)

type AccessVerbPhraseRules :: (Type -> Type -> Type -> Type) -> Type
data AccessVerbPhraseRules r = AccessVerbPhraseRules
  { _toggleVerb             :: Prod r Text Lexeme ToggleVerb
  , _toggleNounPhrase       :: Prod r Text Lexeme ToggleNounPhrase
  , _modToggleVerb          :: Prod r Text Lexeme ModToggleVerb
  , _modToggleNounPhrase    :: Prod r Text Lexeme ModToggleNounPhrase
  , _simpleAccessVerb       :: Prod r Text Lexeme SimpleAccessVerb
  , _simpleAccessNounPhrase :: Prod r Text Lexeme SimpleAccessNounPhrase
  }

accessVerbPhraseRule :: AccessVerbPhraseRules r
                        -> Grammar r (Prod r Text Lexeme AccessVerbPhrase)
accessVerbPhraseRule (AccessVerbPhraseRules {..}) =
  rule $ ToggleVerbPhrase <$> _toggleVerb <*> _toggleNounPhrase
           <|> ModToggleVerbPhrase <$> _modToggleVerb <*> _modToggleNounPhrase
           <|> SimpleAccessVerbPhrase
                 <$> _simpleAccessVerb
                 <*> _simpleAccessNounPhrase

type StimulusVerbPhrase :: Type
data StimulusVerbPhrase
  = SimpleImplicitStimulusVerbPhrase ImplicitStimulusVerb
  | ExplicitStimiulusVerbPhrase ExplicitStimulusVerb DirectionalStimulusNounPhrase
  | TargetedStimulusVerbPhrase TargetedStimulusVerb TargetedStimulusNounPhrase
  | DirectionalStimulusVerbPhrase DirectionalStimulusVerb DirectionalStimulusMarker DirectionalStimulusNounPhrase
  | ContainerStimulusVerbPhrase DirectionalStimulusVerb ContainerPhrase
  | ImplicitRegionalStimulusVerbPhrase ImplicitRegionalStimulusVerb
  deriving stock (Show, Eq, Ord, Generic)

type StimulusVerbPhraseRules :: (Type -> Type -> Type -> Type) -> Type
data StimulusVerbPhraseRules r = StimulusVerbPhraseRules
  { _implicitStimulusVerb          :: Prod r Text Lexeme ImplicitStimulusVerb
  , _explicitStimulusVerb          :: Prod r Text Lexeme ExplicitStimulusVerb
  , _targetedStimulusVerb          :: Prod r Text Lexeme TargetedStimulusVerb
  , _targetedStimulusNounPhrase    :: Prod r Text Lexeme TargetedStimulusNounPhrase
  , _directionalStimulusVerb       :: Prod r Text Lexeme DirectionalStimulusVerb
  , _directionalStimulusMarker     :: Prod r Text Lexeme DirectionalStimulusMarker
  , _directionalStimulusNoun       :: Prod r Text Lexeme DirectionalStimulusNounPhrase
  , _containerPhrase               :: Prod r Text Lexeme ContainerPhrase
  , _implicitRegionalStimulusVerb  :: Prod r Text Lexeme ImplicitRegionalStimulusVerb
  }

stimulusVerbPhraseRule :: StimulusVerbPhraseRules r
                           -> Grammar r (Prod r Text Lexeme StimulusVerbPhrase)
stimulusVerbPhraseRule (StimulusVerbPhraseRules {..}) =
  rule $ SimpleImplicitStimulusVerbPhrase <$> _implicitStimulusVerb
           <|> ExplicitStimiulusVerbPhrase
                <$> _explicitStimulusVerb
                <*> _directionalStimulusNoun
           <|> TargetedStimulusVerbPhrase
                 <$> _targetedStimulusVerb
                 <*> _targetedStimulusNounPhrase
           <|> DirectionalStimulusVerbPhrase
                 <$> _directionalStimulusVerb
                 <*> _directionalStimulusMarker
                 <*> _directionalStimulusNoun
           <|> ContainerStimulusVerbPhrase <$> _directionalStimulusVerb <*> _containerPhrase
           <|> ImplicitRegionalStimulusVerbPhrase <$> _implicitRegionalStimulusVerb

type Imperative :: Type
data Imperative
  = GeneralPlacement GeneralPlacementVerbPhrase -- Drop Lamp
  | AcquisitionalPlacement AcquisitionVerbPhrase -- Take Lamp
  | StimulusVerbPhrase StimulusVerbPhrase -- "Look" "Listen" "Smell" "Taste" "Touch"
  | CardinalMovement CardinalMovementVerb ImplicitPath -- "Go North"
  | AccessVerbPhrase AccessVerbPhrase -- "flip" "push" "pull"
  deriving stock (Show, Eq, Ord, Generic)

type ImperativeRules :: (Type -> Type -> Type -> Type) -> Type
data ImperativeRules r = ImperativeRules
  { _generalPlacementVerbPhrase :: Prod r Text Lexeme GeneralPlacementVerbPhrase
  , _acquisitionVerbPhrase      :: Prod r Text Lexeme AcquisitionVerbPhrase
  , _stimulusVerbPhrase         :: Prod r Text Lexeme StimulusVerbPhrase
  , _cardinalMovementVerb       :: Prod r Text Lexeme CardinalMovementVerb
  , _implicitPath               :: Prod r Text Lexeme ImplicitPath
  , _accessVerbPhrase           :: Prod r Text Lexeme AccessVerbPhrase
  }

imperativeRule :: ImperativeRules r
                  -> Grammar r (Prod r Text Lexeme Imperative)
imperativeRule (ImperativeRules {..}) =
  trace "imperative rule " $ rule $ GeneralPlacement <$> _generalPlacementVerbPhrase
           <|> AcquisitionalPlacement <$> _acquisitionVerbPhrase
           <|> StimulusVerbPhrase <$> _stimulusVerbPhrase
           <|> CardinalMovement <$> _cardinalMovementVerb <*> _implicitPath
           <|> AccessVerbPhrase <$> _accessVerbPhrase

type Interrogative :: Type
data Interrogative
  = ObjectInterrogative ObjectInterrogativeMarker TopicMarker ObjectPhrase -- What about my home?
  | LocationInterrogative LocationInterrogativeMarker Copula ObjectPhrase -- Where is the key?
  deriving stock (Show, Eq, Ord, Generic)

type InterrogativeRules :: (Type -> Type -> Type -> Type) -> Type
data InterrogativeRules r = InterrogativeRules
  { _objectInterrogativeMarker :: Prod r Text Lexeme ObjectInterrogativeMarker
  , _topicMarker               :: Prod r Text Lexeme TopicMarker
  , _locationInterrogativeMarker :: Prod r Text Lexeme LocationInterrogativeMarker
  , _copula                    :: Prod r Text Lexeme Copula
  , _objectPhrase              :: Prod r Text Lexeme ObjectPhrase
  }

interrogativeRule :: InterrogativeRules r
                       -> Grammar r (Prod r Text Lexeme Interrogative)
interrogativeRule (InterrogativeRules {..}) =
  rule $ ObjectInterrogative
             <$> _objectInterrogativeMarker
             <*> _topicMarker
             <*> _objectPhrase
          <|> LocationInterrogative
                <$> _locationInterrogativeMarker
                <*> _copula
                <*> _objectPhrase

type Vocative :: Type
data Vocative
  = VocativeImperative NamedAgent Partition Imperative
  | VocativeInterrogative NamedAgent Partition Interrogative
  deriving stock (Show, Eq, Ord, Generic)

type VocativeRules :: (Type -> Type -> Type -> Type) -> Type
data VocativeRules r = VocativeRules
  { _agent         :: Prod r Text Lexeme NamedAgent
  , _partition     :: Prod r Text Lexeme Partition
  , _imperative    :: Prod r Text Lexeme Imperative
  , _interrogative :: Prod r Text Lexeme Interrogative
  }

vocativeRule :: VocativeRules r
                -> Grammar r (Prod r Text Lexeme Vocative)
vocativeRule (VocativeRules {..}) =
  rule $ VocativeImperative <$> _agent <*> _partition <*> _imperative
           <|> VocativeInterrogative
                 <$> _agent <*> _partition <*> _interrogative

-- Ford, what about my home.
-- Floyd, Go Up
-- Floyd, get shiny board
#ifdef TESTING
deriving via GenericArbitrary TraversalVerbPhrase
  instance Arbitrary TraversalVerbPhrase
deriving via GenericArbitrary GeneralPlacementVerbPhrase
  instance Arbitrary GeneralPlacementVerbPhrase
deriving via GenericArbitrary AcquisitionVerbPhrase
  instance Arbitrary AcquisitionVerbPhrase
deriving via GenericArbitrary AccessVerbPhrase
  instance Arbitrary AccessVerbPhrase
deriving via GenericArbitrary StimulusVerbPhrase
  instance Arbitrary StimulusVerbPhrase
deriving via GenericArbitrary Imperative
  instance Arbitrary Imperative
deriving via GenericArbitrary Interrogative
  instance Arbitrary Interrogative
-- Manual Arbitrary instance for Vocative
-- Replace the DerivingVia line with this if needed:

-- Manual Arbitrary instance for Vocative
-- Replace the DerivingVia line with this if needed:

instance Arbitrary Vocative where
  arbitrary = oneof
    [ VocativeImperative <$> arbitrary <*> pure (Partition SEPERATOR) <*> arbitrary
    , VocativeInterrogative <$> arbitrary <*> pure (Partition SEPERATOR) <*> arbitrary
    ]

  shrink (VocativeImperative agent partition imperative) =
    -- No need to shrink partition since it's always the same value
    [VocativeImperative agent' partition imperative | agent' <- shrink agent] ++
    [VocativeImperative agent partition imperative' | imperative' <- shrink imperative]

  shrink (VocativeInterrogative agent partition interrogative) =
    -- No need to shrink partition since it's always the same value
    [VocativeInterrogative agent' partition interrogative | agent' <- shrink agent] ++
    [VocativeInterrogative agent partition interrogative' | interrogative' <- shrink interrogative]
#endif
