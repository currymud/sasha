module Model.Parser.Atomics.Nouns where

import           Data.Hashable             (Hashable)
import           Data.HashSet              (HashSet, fromList, toList)
import           Data.Kind                 (Type)
import           Lexer
import           Relude.String.Conversion  (ToText)

#ifdef TESTING
import qualified Data.HashSet              as HS
import           Test.QuickCheck           (Arbitrary, elements)
import           Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
#endif

type Objective :: Type
newtype Objective =
  Objective { _fromObjective :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme Objective where
  toLexeme = _fromObjective

type Supportive :: Type
newtype Supportive =
  Supportive { _fromSupportive :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme Supportive where
  toLexeme = _fromSupportive

type Container :: Type
newtype Container =
  Container { _fromContainer :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme Container where
  toLexeme = _fromContainer

type Surface :: Type
newtype Surface = Surface { _fromSurface :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme Surface where
  toLexeme = _fromSurface

type ToggleNoun :: Type
newtype ToggleNoun = ToggleNoun { _fromToggleNoun :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme ToggleNoun where
  toLexeme = _fromToggleNoun

type ModToggleNoun :: Type
newtype ModToggleNoun = ModToggleNoun { _fromModToggleNoun :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme ModToggleNoun where
  toLexeme = _fromModToggleNoun

type SimpleAccessNoun :: Type
newtype SimpleAccessNoun = SimpleAccessNoun { _fromSimpleAccessNoun :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme SimpleAccessNoun where
  toLexeme = _fromSimpleAccessNoun

type InstrumentalAccessNoun :: Type
newtype InstrumentalAccessNoun = InstrumentalAccessNoun { _fromInstrumentalAccessNoun :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord)

instance HasLexeme InstrumentalAccessNoun where
  toLexeme = _fromInstrumentalAccessNoun

type Switch :: Type
newtype Switch = Switch { _fromSwitch :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord)

instance HasLexeme Switch where
  toLexeme = _fromSwitch

type Instrumental :: Type
newtype Instrumental = Instrumental { _fromInstrumental :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord)

instance HasLexeme Instrumental where
  toLexeme = _fromInstrumental

type ProcessableDevice :: Type
newtype ProcessableDevice = ProcessableDevice { _fromProcessableDevice :: Lexeme }
  deriving newtype (Show, Eq, Hashable, Ord)

instance HasLexeme ProcessableDevice where
  toLexeme = _fromProcessableDevice

type ObjectPath :: Type
newtype ObjectPath = ObjectPath { _fromObjectPath :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord, ToText)

instance HasLexeme ObjectPath where
  toLexeme = _fromObjectPath

type LReferentials :: Type
newtype LReferentials = LReferentials { _fromLReferentials :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord)

instance HasLexeme LReferentials where
  toLexeme = _fromLReferentials

type DirectionalStimulus :: Type
newtype DirectionalStimulus = DirectionalStimulus { _fromDirectionalStimulus :: Lexeme }
  deriving newtype (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme DirectionalStimulus where
  toLexeme = _fromDirectionalStimulus

-- nouns that need 'to' to be used with them with sensory verbs
-- e.g. "listen to the music"
type TargetedStimulus :: Type
newtype TargetedStimulus = TargetedStimulus { _fromTargetedStimulus :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (ToText,Hashable)

instance HasLexeme TargetedStimulus where
  toLexeme = _fromTargetedStimulus

type Region :: Type
newtype Region = Region { _fromRegion :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord)

instance HasLexeme Region where
  toLexeme = _fromRegion

type TReferentials :: Type
newtype TReferentials = TReferentials { _fromTReferentials :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord)

instance HasLexeme TReferentials where
  toLexeme = _fromTReferentials

type Agent :: Type
newtype Agent = Agent { _fromAgent :: Lexeme }
  deriving newtype (Show, Eq, Hashable, Ord)

instance HasLexeme Agent where
  toLexeme = _fromAgent

type NamedAgent :: Type
newtype NamedAgent = NamedAgent { _fromNamedAgent :: Lexeme }
  deriving stock (Show, Eq, Ord)
  deriving newtype (Hashable, ToText)

instance HasLexeme NamedAgent where
  toLexeme = _fromNamedAgent

type ReferenceMaterial :: Type
newtype ReferenceMaterial = ReferenceMaterial { _fromReferenceMaterial :: Lexeme }
  deriving newtype (Show, Eq, Hashable, Ord)

instance HasLexeme ReferenceMaterial where
  toLexeme = _fromReferenceMaterial

#ifdef TESTING
instance Arbitrary Objective where
  arbitrary = elements $ HS.toList objectives

instance Arbitrary Supportive where
  arbitrary = elements $ HS.toList supportives

instance Arbitrary Container where
  arbitrary = elements $ HS.toList containers

instance Arbitrary Surface where
  arbitrary = elements $ HS.toList surfaces

instance Arbitrary ToggleNoun where
  arbitrary = elements $ HS.toList toggleNouns

instance Arbitrary ModToggleNoun where
  arbitrary = elements $ HS.toList modToggleNouns

instance Arbitrary SimpleAccessNoun where
  arbitrary = elements $ HS.toList simpleAccessNouns

instance Arbitrary InstrumentalAccessNoun where
  arbitrary = elements $ HS.toList instrumentalAccessNouns

instance Arbitrary Switch where
  arbitrary = elements $ HS.toList switches

instance Arbitrary Instrumental where
  arbitrary = elements $ HS.toList instrumentals

instance Arbitrary ProcessableDevice where
  arbitrary = elements $ HS.toList processableDevices

instance Arbitrary ObjectPath where
  arbitrary = elements $ HS.toList objectPaths

instance Arbitrary LReferentials where
  arbitrary = elements $ HS.toList lreferentials

instance Arbitrary TargetedStimulus where
  arbitrary = elements $ HS.toList targetedStimulii

instance Arbitrary DirectionalStimulus where
  arbitrary = elements $ HS.toList directionalStimulii

instance Arbitrary Region where
  arbitrary = elements $ HS.toList regions

instance Arbitrary TReferentials where
  arbitrary = elements $ HS.toList treferentials

instance Arbitrary Agent where
  arbitrary = elements $ HS.toList agents

instance Arbitrary NamedAgent where
  arbitrary = elements $ HS.toList namedAgents

instance Arbitrary ReferenceMaterial where
  arbitrary = elements $ HS.toList referenceMaterials
#endif
