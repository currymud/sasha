{-# OPTIONS_GHC -Wno-missed-specialisations #-}
module Grammar.Model.Lexer where

import           Control.Applicative        (Alternative, (<|>))
import           Control.Monad              (MonadPlus, void)
import           Data.Hashable              (Hashable, hashUsing, hashWithSalt)
import           Data.Kind                  (Type)
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import           Relude.String.Conversion   (ToString (toString),
                                             ToText (toText))
import           Text.Megaparsec            (MonadParsec, Parsec)
import           Text.Megaparsec.Char       (spaceChar)
import qualified Text.Megaparsec.Char.Lexer as L
#ifdef TESTING
import           Test.QuickCheck            (Arbitrary (..),
                                             arbitraryBoundedEnum)
#endif
type Lexer :: Type -> Type
newtype Lexer a = Lexer {runLexer :: Parsec Void Text a}
  deriving newtype (Functor, Applicative, Alternative,Monad,MonadPlus)

type Lexeme :: Type
data Lexeme
  = FROM
  | ENTER
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
  | ONTO
  | IN
  | TO
  | WHEN
  | UNDER
  | OVER
  | ABOVE
  | AT
  | ON
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


