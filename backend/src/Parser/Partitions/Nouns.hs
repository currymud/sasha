module Parser.Partitions.Nouns where

import           Data.Hashable            (Hashable)
import           Data.HashSet             (HashSet, fromList, toList)
import           Data.Kind                (Type)
import           Lexer
import           Relude.String.Conversion (ToText)

-- Nouns that can have their state changed
objectives :: HashSet Objective
objectives =
  fromList $ map Objective [BELL, BOOK, BUTTON, CANDLE, DOOR, LAMP, PLANT, POT
                           , TEA, HOME, BED, CARD, BOX, MAN, BALL]

-- Nouns that can have things put on them
supportives :: HashSet Supportive
supportives = fromList $ map Supportive [SHELF, BOX, TABLE,SATCHEL]

-- Nouns that can have things put in them
containers :: HashSet Container
containers = fromList
  $ map Container [BOX, CABINET, BAG, POT, LAMP,SINK,SATCHEL, THING, POCKET]

-- Nouns that can have things put on them, but were not designed to be supports. i.e. "put sticker on wall"
surfaces :: HashSet Surface
surfaces = fromList $ map Surface [WALL, CEILING, DOOR,SHELF]

toggleNouns :: HashSet ToggleNoun
toggleNouns = fromList $ map ToggleNoun [SWITCH, BUTTON]

modToggleNouns :: HashSet ModToggleNoun
modToggleNouns = fromList $ map ModToggleNoun [LIGHT,LAMP]

simpleAccessNouns :: HashSet SimpleAccessNoun
simpleAccessNouns = fromList $ map SimpleAccessNoun [DOOR,CABINET,SHELF,BOX]

instrumentalAccessNouns :: HashSet InstrumentalAccessNoun
instrumentalAccessNouns = fromList $ map InstrumentalAccessNoun [KEY,CARD]

switches :: HashSet Switch
switches = fromList $ map Switch [BUTTON,SWITCH]

-- Nouns used to do a thing (Unlock door with key). Key would be Instrumental Case
instrumentals :: HashSet Instrumental
instrumentals = fromList $ map Instrumental [KEY, CANDLE, TROWEL,CARD]

processableDevices :: HashSet ProcessableDevice
processableDevices = fromList $ map ProcessableDevice [READER, SCANNER, SLOT]

-- Nouns that can signify direction

objectPaths :: HashSet ObjectPath
objectPaths = fromList $ map ObjectPath [WINDOW,PORTAL,ATTIC,DOOR]

-- Nouns that can have things located relative to it -- hide the key behind the picture. picture being Referential :: Locative
lreferentials :: HashSet LReferentials
lreferentials = fromList $ map LReferentials [SHELF,SINK,CABINET] -- under the sink, above the cabinet

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

targetedStimulii :: HashSet TargetedStimulus
targetedStimulii = fromList
  $ map TargetedStimulus ([MUSIC, SOUND, RADIO, WHISPER] <> alsoAgents)
  where
    alsoAgents = lexemes agents

lexemes :: (HasLexeme a) => HashSet a -> [Lexeme]
lexemes speechParts = toLexeme <$> toList speechParts

-- Nouns that describe a bounded region of space. Region :: Locative

regions :: HashSet Region
regions = fromList
  $ map Region [PALACE, BASEMENT, ATTIC, MAZE, DEN, PARLOUR, VOIDLESSVOID, BED]

-- Nouns that can be asked about Referential :: Topic
treferentials :: HashSet TReferentials
treferentials = fromList $ TReferentials <$>
  lexemes objectives
  <> lexemes supportives
  <> lexemes containers
  <> lexemes instrumentals
  <> lexemes objectPaths

agents :: HashSet Agent
agents = fromList $ map Agent [MAN,WOMAN,PERSON,GUARD]

namedAgents :: HashSet NamedAgent
namedAgents = fromList $ map NamedAgent [FORD, WILLIAM, FLOYD]

referenceMaterials :: HashSet ReferenceMaterial
referenceMaterials = fromList $ map ReferenceMaterial [BOOK, JOURNAL, RECORDS, ENCYCLOPEDIA, GUIDE]
