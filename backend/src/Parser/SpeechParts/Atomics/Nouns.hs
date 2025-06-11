module Parser.SpeechParts.Atomics.Nouns where

import           Data.Hashable (Hashable)
import           Data.HashSet  (HashSet, fromList, singleton, toList)
import           Data.Kind     (Type)
import           Lexer

type Objective :: Type
newtype Objective =
  Objective { _fromObjective :: Lexeme } deriving newtype (Show,Eq,Hashable,Ord)

instance HasLexeme Objective where
  toLexeme = _fromObjective

-- Nouns that can have their state changed
objectives :: HashSet Objective
objectives =
  fromList $ map Objective [BELL, BOOK, BUTTON, CANDLE, DOOR, LAMP, PLANT, POT
                           , TEA, HOME, BED, CARD, BOX, MAN, BALL]

type Supportive :: Type
newtype Supportive =
  Supportive { _fromSupportive :: Lexeme } deriving newtype (Show,Eq,Hashable,Ord)

instance HasLexeme Supportive where
  toLexeme = _fromSupportive

-- Nouns that can have things put on them
supportives :: HashSet Supportive
supportives = fromList $ map Supportive [SHELF, BOX, TABLE,SATCHEL]

type Container :: Type
newtype Container =
  Container { _fromContainer :: Lexeme } deriving newtype (Show,Eq,Hashable,Ord)

instance HasLexeme Container where
  toLexeme = _fromContainer

-- Nouns that can have things put in them
containers :: HashSet Container
containers = fromList
  $ map Container [BOX, CABINET, BAG, POT, LAMP,SINK,SATCHEL, THING, POCKET]

type Surface :: Type
newtype Surface = Surface { _fromSurface :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord)

instance HasLexeme Surface where
  toLexeme = _fromSurface

-- Nouns that can have things put on them, but were not designed to be supports. i.e. "put sticker on wall"
surfaces :: HashSet Surface
surfaces = fromList $ map Surface [WALL, CEILING, DOOR,SHELF]

type ToggleNoun :: Type
newtype ToggleNoun = ToggleNoun { _fromToggleNoun :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord)

instance HasLexeme ToggleNoun where
  toLexeme = _fromToggleNoun

toggleNouns :: HashSet ToggleNoun
toggleNouns = fromList $ map ToggleNoun [SWITCH, BUTTON]

type ModToggleNoun :: Type
newtype ModToggleNoun = ModToggleNoun { _fromModToggleNoun :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord)

instance HasLexeme ModToggleNoun where
  toLexeme = _fromModToggleNoun

modToggleNouns :: HashSet ModToggleNoun
modToggleNouns = fromList $ map ModToggleNoun [LIGHT,LAMP]

type SimpleAccessNoun :: Type
newtype SimpleAccessNoun = SimpleAccessNoun { _fromSimpleAccessNoun :: Lexeme }
  deriving newtype (Show,Eq,Hashable,Ord)

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
  deriving newtype (Show,Eq,Hashable,Ord)

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
  deriving newtype (Show,Eq,Hashable,Ord)

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
  deriving newtype (Show,Eq,Hashable,Ord)

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
  deriving newtype (Show, Eq, Hashable, Ord)

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
