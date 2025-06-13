module Lexer.Model where

import           Control.Applicative        ((<|>))
import           Control.Monad              (void)
import           Data.Hashable              (Hashable, hashUsing, hashWithSalt)
import           Data.HashSet               as HS
import           Data.Kind                  (Type)
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import           Relude.String.Conversion   (ToString (toString),
                                             ToText (toText))
import           Text.Megaparsec            (Parsec)
import           Text.Megaparsec.Char       (spaceChar)
import qualified Text.Megaparsec.Char.Lexer as L
#ifdef TESTING
import           Test.QuickCheck            (Arbitrary (..),
                                             arbitraryBoundedEnum)
#endif

type Parser = Parsec Void String


symbol :: String -> Parser String
symbol = L.symbol sc

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

type Lexeme :: Type
data Lexeme
  = FROM
  | SEPERATOR
  | CARD
  | READER
  | SCANNER
  | SLOT
  | ABOUT
  | ACROSS
  | AROUND
  | BETWEEN
  | FLOOR
  | SMALL
  | LARGE
  | OFF
  | OPEN
  | CLOSE
  | KITCHEN
  | CABINET
  | DOOR
  | ALL
  | EXAMINE
  | PALACE
  | BASEMENT
  | ATTIC
  | MAZE
  | MOVE
  | DEN
  | PARLOUR
  | VOIDLESSVOID
  | PLANT
  | POT
  | BOOK
  | BELL
  | CANDLE
  | TEA
  | LOOK
  | PUT
  | THAT
  | THIS
  | THE
  | A
  | MY
  | ME
  | WITH
  | INTO
  | IN
  | TO
  | WHEN
  | UNDER
  | OVER
  | ABOVE
  | AT
  | ON
  | ONTO
  | MIND
  | BLUE
  | RED
  | GREAT
  | LONG
  | OLD
  | DRUNK
  | FORD
  | WILLIAM
  | ONE
  | LAMP
  | TWO
  | THREE
  | All
  | NORTH
  | EAST
  | SOUTH
  | WEST
  | DOWN
  | GET
  | SINK
  | THROUGH
  | SOIL
  | WATERING
  | CAN
  | BAG
  | UP
  | CLIMB
  | GO
  | HALL
  | MARQUEE
  | SHELF
  | LOCKED
  | UNLOCKED
  | VERBOSE
  | VISIBLE
  | LEFT
  | RIGHT
  | FRONT
  | BEHIND
  | BACK
  | PORTAL
  | TROWEL
  | BUTTON
  | BOX
  | TABLE
  | ROBE
  | SATCHEL
  | THING
  | MAIL
  | POCKET
  | KEY
  | WINDOW
  | WALL
  | CEILING
  | SLEEP
  | WAIT
  | JUMP
  | SAIL
  | SNEAK
  | RUN
  | MARCH
  | FLOAT
  | FLEE
  | CRAWL
  | SWIM
  | ENTER
  | EXIT
  | THROW
  | SHOOT
  | TOSS
  | PUSH
  | PULL
  | TURN
  | TWIST
  | ROTATE
  | SLIDE
  | INSERT
  | REMOVE
  | DROP
  | SMELL
  | TASTE
  | LISTEN
  | TOUCH
  | BED
  | LOCK
  | UNLOCK
  | CLOCKWISE
  | COUNTERCLOCKWISE
  | ASK
  | TELL
  | SAY
  | GIVE
  | TAKE
  | SHOW
  | OPENQUOTE
  | CLOSEQUOTE
  | WHAT
  | WALK
  | HOME
  | IS
  | WHO
  | WHERE
  | FLOYD
  | BY
  | PLAY
  | RING
  | FLIP
  | SWITCH
  | LIGHT
  | MUSIC
  | SOUND
  | RADIO
  | WHISPER
  | MAN
  | WOMAN
  | PERSON
  | GUARD
  | JOURNAL
  | SCROll
  | RECORDS
  | ENCYCLOPEDIA
  | GUIDE
  | BALL
  deriving stock (Show, Bounded, Eq, Enum, Ord)

#ifdef TESTING
instance Arbitrary Lexeme where
  arbitrary = arbitraryBoundedEnum
#endif

instance ToText Lexeme where
  toText :: Lexeme -> Text
  toText SEPERATOR  = ","
  toText OPENQUOTE  = "\""
  toText CLOSEQUOTE = "\""
  toText txt        = toText (show txt :: String)

instance ToString Lexeme where
  toString :: Lexeme -> String
  toString = toString . toText

instance Hashable Lexeme where
  hashWithSalt :: Int -> Lexeme -> Int
  hashWithSalt = hashUsing fromEnum

term :: Parser Lexeme
term =
  PORTAL <$ symbol "PORTAL"
    <|> SEPERATOR <$ symbol ","
    <|> PALACE <$ symbol "PALACE"
    <|> ACROSS <$ symbol "ACROSS"
    <|> ABOUT <$ symbol "ABOUT"
    <|> AROUND <$ symbol "AROUND"
    <|> FROM <$ symbol "FROM"
    <|> floor'
    <|> small
    <|> large
    <|> BASEMENT <$ symbol "BASEMENT"
    <|> ATTIC <$ symbol "ATTIC"
    <|> MAZE <$ symbol "MAZE"
    <|> DEN <$ symbol "DEN"
    <|> PARLOUR <$ symbol "PARLOUR"
    <|> VOIDLESSVOID <$ symbol "VOIDLESSVOID"
    <|> PLANT <$ symbol "PLANT"
    <|> POT <$ symbol "POT"
    <|> BOOK <$ symbol "BOOK"
    <|> BELL <$ symbol "BELL"
    <|> CANDLE <$ symbol "CANDLE"
    <|> TEA <$ symbol "TEA"
    <|> LOOK <$ symbol "LOOK"
    <|> put
    <|> THAT <$ symbol "THAT"
    <|> THIS <$ symbol "THIS"
    <|> THE <$ symbol "THE"
    <|> AT <$ symbol "AT"
    <|> MOVE <$ symbol "MOVE"
    <|> MY <$ symbol "MY"
    <|> ME <$ symbol "ME"
    <|> TOUCH <$ symbol "TOUCH"
    <|> TOSS <$ symbol "TOSS"
    <|> TO <$ symbol "TO"
    <|> WITH <$ symbol "WITH"
    <|> INTO <$ symbol "INTO"
    <|> INSERT <$ symbol "INSERT"
    <|> IN <$ symbol "IN"
    <|> WHEN <$ symbol "WHEN"
    <|> under
    <|> OVER <$ symbol "OVER"
    <|> ONE <$ symbol "ONE"
    <|> ON <$ symbol "ON"
    <|> MIND <$ symbol "MIND"
    <|> BLUE <$ symbol "BLUE"
    <|> RED <$ symbol "RED"
    <|> GREAT <$ symbol "GREAT"
    <|> LONG <$ symbol "LONG"
    <|> OLD <$ symbol "OLD"
    <|> DRUNK <$ symbol "DRUNK"
    <|> FORD <$ symbol "FORD"
    <|> WILLIAM <$ symbol "WILLIAM"
    <|> TWO <$ symbol "TWO"
    <|> THREE <$ symbol "THREE"
    <|> ALL <$ symbol "ALL"
    <|> NORTH <$ symbol "NORTH"
    <|> EAST <$ symbol "EAST"
    <|> SOUTH <$ symbol "SOUTH"
    <|> WEST <$ symbol "WEST"
    <|> UP <$ symbol "UP"
    <|> DOWN <$ symbol "DOWN"
    <|> EXAMINE <$ symbol "EXAMINE"
    <|> get
    <|> OPEN <$ symbol "OPEN"
    <|> CABINET <$ symbol "CABINET"
    <|> DOOR <$ symbol "DOOR"
    <|> SINK <$ symbol "SINK"
    <|> KITCHEN <$ symbol "KITCHEN"
    <|> THROUGH <$ symbol "THROUGH"
    <|> SOIL <$ symbol "SOIL"
    <|> RING <$ symbol "RING"
    <|> WATERING <$ symbol "WATERING"
    <|> CAN <$ symbol "CAN"
    <|> BAG <$ symbol "BAG"
    <|> go
    <|> close
    <|> CLIMB <$ symbol "CLIMB"
    <|> HALL <$ symbol "HALL"
    <|> MARQUEE <$ symbol "MARQUEE"
    <|> SHELF <$ symbol "SHELF"
    <|> LOCKED <$ symbol "LOCKED"
    <|> UNLOCKED <$ symbol "UNLOCKED"
    <|> VERBOSE <$ symbol "VERBOSE"
    <|> VISIBLE <$ symbol "VISIBLE"
    <|> LEFT <$ symbol "LEFT"
    <|> RIGHT <$ symbol "RIGHT"
    <|> FRONT <$ symbol "FRONT"
    <|> BEHIND <$ symbol "BEHIND"
    <|> SINK <$ symbol "SINK"
    <|> BACK <$ symbol "BACK"
    <|> ABOVE <$ symbol "ABOVE"
    <|> ASK <$ symbol "ASK"
    <|> A <$ symbol "A"
    <|> LAMP <$ symbol "LAMP"
    <|> TROWEL <$ symbol "TROWEL"
    <|> BUTTON <$ symbol "BUTTON"
    <|> BOX <$ symbol "BOX"
    <|> TABLE <$ symbol "TABLE"
    <|> ROBE <$ symbol "ROBE"
    <|> SATCHEL <$ symbol "SATCHEL"
    <|> THING <$ symbol "THING"
    <|> MAIL <$ symbol "MAIL"
    <|> POCKET <$ symbol "POCKET"
    <|> KEY <$ symbol "KEY"
    <|> WINDOW <$ symbol "WINDOW"
    <|> WALL <$ symbol "WALL"
    <|> CEILING <$ symbol "CEILING"
    <|> SLEEP <$ symbol "SLEEP"
    <|> WAIT <$ symbol "WAIT"
    <|> ONTO <$ symbol "ONTO"
    <|> BETWEEN <$ symbol "BETWEEN"
    <|> OFF <$ symbol "OFF"
    <|> JUMP <$ symbol "JUMP"
    <|> SAIL <$ symbol "SAIL"
    <|> SNEAK <$ symbol "SNEAK"
    <|> RUN <$ symbol "RUN"
    <|> MARCH <$ symbol "MARCH"
    <|> FLOAT <$ symbol "FLOAT"
    <|> FLEE <$ symbol "FLEE"
    <|> CRAWL <$ symbol "CRAWL"
    <|> SWIM <$ symbol "SWIM"
    <|> ENTER <$ symbol "ENTER"
    <|> EXIT <$ symbol "EXIT"
    <|> THROW <$ symbol "THROW"
    <|> SHOOT <$ symbol "SHOOT"
    <|> push
    <|> PULL <$ symbol "PULL"
    <|> TURN <$ symbol "TURN"
    <|> TWIST <$ symbol "TWIST"
    <|> ROTATE <$ symbol "ROTATE"
    <|> SLIDE <$ symbol "SLIDE"
    <|> REMOVE <$ symbol "REMOVE"
    <|> DROP <$ symbol "DROP"
    <|> SMELL <$ symbol "SMELL"
    <|> TASTE <$ symbol "TASTE"
    <|> LISTEN <$ symbol "LISTEN"
    <|> BED <$ symbol "BED"
    <|> LOCK <$ symbol "LOCK"
    <|> UNLOCK <$ symbol "UNLOCK"
    <|> CLOCKWISE <$ symbol "CLOCKWISE"
    <|> COUNTERCLOCKWISE <$ symbol "COUNTERCLOCKWISE"
    <|> TELL <$ symbol "TELL"
    <|> SAY <$ symbol "SAY"
    <|> GIVE <$ symbol "GIVE"
    <|> TAKE <$ symbol "TAKE"
    <|> SHOW <$ symbol "SHOW"
    <|> OPENQUOTE <$ symbol "\""
    <|> CLOSEQUOTE <$ symbol "\""
    <|> WHAT <$ symbol "WHAT"
    <|> WALK <$ symbol "WALK"
    <|> HOME <$ symbol "HOME"
    <|> IS <$ symbol "IS"
    <|> WHO <$ symbol "WHO"
    <|> WHERE <$ symbol "WHERE"
    <|> FLOYD <$ symbol "FLOYD"
    <|> BY <$ symbol "BY"
    <|> READER <$ symbol "READER"
    <|> SCANNER <$ symbol "SCANNER"
    <|> SLOT <$ symbol "SLOT"
    <|> CARD <$ symbol "CARD"
    <|> PLAY <$ symbol "PLAY"
    <|> SWITCH <$ symbol "SWITCH"
    <|> FLIP <$ symbol "FLIP"
    <|> LIGHT <$ symbol "LIGHT"
    <|> MUSIC <$ symbol "MUSIC"
    <|> SOUND <$ symbol "SOUND"
    <|> RADIO <$ symbol "RADIO"
    <|> WHISPER <$ symbol "WHISPER"
    <|> MAN <$ symbol "MAN"
    <|> WOMAN <$ symbol "WOMAN"
    <|> PERSON <$ symbol "PERSON"
    <|> GUARD <$ symbol "GUARD"
    <|> JOURNAL <$ symbol "JOURNAL"
    <|> SCROll <$ symbol "SCROLL"
    <|> RECORDS <$ symbol "RECORDS"
    <|> ENCYCLOPEDIA <$ symbol "ENCYCLOPEDIA"
    <|> GUIDE <$ symbol "GUIDE"
    <|> BALL <$ symbol "BALL"

push :: Parser Lexeme
push = PUSH <$ symbol "PUSH" <|> PUSH <$ symbol "PRESS"
put :: Parser Lexeme
put = PUT <$ symbol "PUT" <|> PUT <$ symbol "PLACE"

under :: Parser Lexeme
under = UNDER <$ symbol "UNDER" <|> UNDER <$ symbol "BELOW"

large :: Parser Lexeme
large = LARGE <$ symbol "LARGE" <|> LARGE <$ symbol "BIG"

small :: Parser Lexeme
small =
  SMALL <$ symbol "SMALL"
    <|> SMALL <$ symbol "TINY"
    <|> SMALL <$ symbol "LITTLE"

floor' :: Parser Lexeme
floor' =
  FLOOR <$ symbol "FLOOR" <|> FLOOR <$ symbol "GROUND"

get :: Parser Lexeme
get = GET <$ symbol "GET" <|> GET <$ symbol "TAKE"

close :: Parser Lexeme
close = CLOSE <$ symbol "CLOSE" <|> CLOSE <$ symbol "SHUT"

go :: Parser Lexeme
go = GO <$ symbol "GO" <|>  GO <$ symbol "ENTER"
