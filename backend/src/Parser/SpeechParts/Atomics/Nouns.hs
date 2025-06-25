module Parser.SpeechParts.Atomics.Nouns where

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

-- Nouns that can have their state changed
objectives :: HashSet Objective
objectives =
  fromList $ map Objective [BELL, BOOK, BUTTON, CANDLE, DOOR, LAMP, PLANT, POT
                           , TEA, HOME, BED, CARD, BOX, MAN, BALL]

type Supportive :: Type
newtype Supportive =
  Supportive { _fromSupportive :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme Supportive where
  toLexeme = _fromSupportive

-- Nouns that can have things put on them
supportives :: HashSet Supportive
supportives = fromList $ map Supportive [SHELF, BOX, TABLE,SATCHEL]

type Container :: Type
newtype Container =
  Container { _fromContainer :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme Container where
  toLexeme = _fromContainer

-- Nouns that can have things put in them
containers :: HashSet Container
containers = fromList
  $ map Container [BOX, CABINET, BAG, POT, LAMP,SINK,SATCHEL, THING, POCKET]

type Surface :: Type
newtype Surface = Surface { _fromSurface :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme Surface where
  toLexeme = _fromSurface

-- Nouns that can have things put on them, but were not designed to be supports. i.e. "put sticker on wall"
surfaces :: HashSet Surface
surfaces = fromList $ map Surface [WALL, CEILING, DOOR,SHELF]

type ToggleNoun :: Type
newtype ToggleNoun = ToggleNoun { _fromToggleNoun :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme ToggleNoun where
  toLexeme = _fromToggleNoun

toggleNouns :: HashSet ToggleNoun
toggleNouns = fromList $ map ToggleNoun [SWITCH, BUTTON]

type ModToggleNoun :: Type
newtype ModToggleNoun = ModToggleNoun { _fromModToggleNoun :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme ModToggleNoun where
  toLexeme = _fromModToggleNoun

modToggleNouns :: HashSet ModToggleNoun
modToggleNouns = fromList $ map ModToggleNoun [LIGHT,LAMP]

type SimpleAccessNoun :: Type
newtype SimpleAccessNoun = SimpleAccessNoun { _fromSimpleAccessNoun :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme SimpleAccessNoun where
  toLexeme = _fromSimpleAccessNoun

simpleAccessNouns :: HashSet SimpleAccessNoun
simpleAccessNouns = fromList $ map SimpleAccessNoun [DOOR,CABINET,SHELF,BOX]

type InstrumentalAccessNoun :: Type
newtype InstrumentalAccessNoun = InstrumentalAccessNoun { _fromInstrumentalAccessNoun :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord)

instance HasLexeme InstrumentalAccessNoun where
  toLexeme = _fromInstrumentalAccessNoun

instrumentalAccessNouns :: HashSet InstrumentalAccessNoun
instrumentalAccessNouns = fromList $ map InstrumentalAccessNoun [KEY,CARD]

type Switch :: Type
newtype Switch = Switch { _fromSwitch :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord)

instance HasLexeme Switch where
  toLexeme = _fromSwitch

switches :: HashSet Switch
switches = fromList $ map Switch [BUTTON,SWITCH]

type Instrumental :: Type
newtype Instrumental = Instrumental { _fromInstrumental :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord)

instance HasLexeme Instrumental where
  toLexeme = _fromInstrumental

-- Nouns used to do a thing (Unlock door with key). Key would be Instrumental Case
instrumentals :: HashSet Instrumental
instrumentals = fromList $ map Instrumental [KEY, CANDLE, TROWEL,CARD]

type ProcessableDevice :: Type
newtype ProcessableDevice = ProcessableDevice { _fromProcessableDevice :: Lexeme }
  deriving newtype (Show, Eq, Hashable, Ord)

instance HasLexeme ProcessableDevice where
  toLexeme = _fromProcessableDevice

processableDevices :: HashSet ProcessableDevice
processableDevices = fromList $ map ProcessableDevice [READER, SCANNER, SLOT]

type ObjectPath :: Type
newtype ObjectPath = ObjectPath { _fromObjectPath :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord, ToText)

instance HasLexeme ObjectPath where
  toLexeme = _fromObjectPath

-- Nouns that can signify direction

objectPaths :: HashSet ObjectPath
objectPaths = fromList $ map ObjectPath [WINDOW,PORTAL,ATTIC,DOOR]

type LReferentials :: Type
newtype LReferentials = LReferentials { _fromLReferentials :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord)

instance HasLexeme LReferentials where
  toLexeme = _fromLReferentials

-- Nouns that can have things located relative to it -- hide the key behind the picture. picture being Referential :: Locative
lreferentials :: HashSet LReferentials
lreferentials = fromList $ map LReferentials [SHELF,SINK,CABINET] -- under the sink, above the cabinet

type DirectionalStimulus :: Type
newtype DirectionalStimulus = DirectionalStimulus { _fromDirectionalStimulus :: Lexeme }
  deriving newtype (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme DirectionalStimulus where
  toLexeme = _fromDirectionalStimulus

-- Nouns that can be looked at
directionalStimulii :: HashSet DirectionalStimulus
directionalStimulii = fromList $ DirectionalStimulus <$>
  lexemes objectives
  <> lexemes supportives
  <> lexemes containers
  <> lexemes instrumentals
  <> lexemes objectPaths
  <> lexemes surfaces
  <> lexemes lreferentials

-- nouns that need 'to' to be used with them with sensory verbs
-- e.g. "listen to the music"
type TargetedStimulus :: Type
newtype TargetedStimulus = TargetedStimulus { _fromTargetedStimulus :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (ToText,Hashable)

instance HasLexeme TargetedStimulus where
  toLexeme = _fromTargetedStimulus

targetedStimulii :: HashSet TargetedStimulus
targetedStimulii = fromList
  $ map TargetedStimulus ([MUSIC, SOUND, RADIO, WHISPER] <> alsoAgents)
  where
    alsoAgents = lexemes agents

lexemes :: (HasLexeme a) => HashSet a -> [Lexeme]
lexemes speechParts = toLexeme <$> toList speechParts

type Region :: Type
newtype Region = Region { _fromRegion :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord)

instance HasLexeme Region where
  toLexeme = _fromRegion
-- Nouns that describe a bounded region of space. Region :: Locative

regions :: HashSet Region
regions = fromList
  $ map Region [PALACE, BASEMENT, ATTIC, MAZE, DEN, PARLOUR, VOIDLESSVOID, BED]

type TReferentials :: Type
newtype TReferentials = TReferentials { _fromTReferentials :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord)

instance HasLexeme TReferentials where
  toLexeme = _fromTReferentials

-- Nouns that can be asked about Referential :: Topic
treferentials :: HashSet TReferentials
treferentials = fromList $ TReferentials <$>
  lexemes objectives
  <> lexemes supportives
  <> lexemes containers
  <> lexemes instrumentals
  <> lexemes objectPaths

-- Not sure what TSOURCE IS FOR
type TSource :: Type
newtype TSource = TSource { _fromTSource :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord)

instance HasLexeme TSource where
  toLexeme = _fromTSource

type Agent :: Type
newtype Agent = Agent { _fromAgent :: Lexeme }
  deriving newtype (Show, Eq, Hashable, Ord)

instance HasLexeme Agent where
  toLexeme = _fromAgent

agents :: HashSet Agent
agents = fromList $ map Agent [MAN,WOMAN,PERSON,GUARD]

type NamedAgent :: Type
newtype NamedAgent = NamedAgent { _fromNamedAgent :: Lexeme }
  deriving stock (Show, Eq, Ord)
  deriving newtype (Hashable, ToText)

instance HasLexeme NamedAgent where
  toLexeme = _fromNamedAgent

namedAgents :: HashSet NamedAgent
namedAgents = fromList $ map NamedAgent [FORD, WILLIAM, FLOYD]

type ReferenceMaterial :: Type
newtype ReferenceMaterial = ReferenceMaterial { _fromReferenceMaterial :: Lexeme }
  deriving newtype (Show, Eq, Hashable, Ord)

instance HasLexeme ReferenceMaterial where
  toLexeme = _fromReferenceMaterial

referenceMaterials :: HashSet ReferenceMaterial
referenceMaterials = fromList $ map ReferenceMaterial [BOOK, JOURNAL, RECORDS, ENCYCLOPEDIA, GUIDE]


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
