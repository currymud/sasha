{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Grammar.Parser.Lexer (
    Model.Parser.Lexer.Lexeme (..)
  , Grammar.Parser.Lexer.HasLexeme (..)
  , Grammar.Parser.Lexer.lexify
  , Grammar.Parser.Lexer.tokens
  ) where
import           Control.Applicative        (Alternative (many), (<|>))
import           Control.Monad              (void)
import           Data.Kind                  (Constraint, Type)
import           Data.Text                  (Text, toUpper)
import           Data.Void                  (Void)
import           Model.Parser.Lexer
import           Relude.String.Conversion   (ToText (toText))
import           Text.Megaparsec            (Parsec, eof, errorBundlePretty,
                                             parse)
import           Text.Megaparsec.Char       (spaceChar)
import qualified Text.Megaparsec.Char.Lexer as L

lexify :: Lexer a -> Text -> Either Text a
lexify (Lexer parser) txt = do
  case parse parser "" str' of
    Left err   -> Left $ toText $ errorBundlePretty err
    Right prog -> Right prog
  where
    str' = toUpper txt

symbol :: Text -> Lexer Text
symbol = Lexer . L.symbol (runLexer sc)

sc ::Lexer ()
sc = Lexer $ L.space (void spaceChar) lineCmnt blockCmnt
  where
    lineCmnt :: Parsec Void Text ()
    lineCmnt = L.skipLineComment "//"
    blockCmnt :: Parsec Void Text ()
    blockCmnt = L.skipBlockComment "/*" "*/"

term :: Lexer Lexeme
term =
  PORTAL <$ symbol "PORTAL"
    <|> CHAIR <$ symbol "CHAIR"
    <|> WHITE <$ symbol "WHITE"
    <|> PILL <$ symbol "PILL"
    <|> TOWEL <$ symbol "TOWEL"
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
    <|> ONTO <$ symbol "ONTO"
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


push :: Lexer Lexeme
push = PUSH <$ symbol "PUSH" <|> PUSH <$ symbol "PRESS"
put :: Lexer Lexeme
put = PUT <$ symbol "PUT" <|> PUT <$ symbol "PLACE"

under :: Lexer Lexeme
under = UNDER <$ symbol "UNDER" <|> UNDER <$ symbol "BELOW"

large :: Lexer Lexeme
large = LARGE <$ symbol "LARGE" <|> LARGE <$ symbol "BIG"

small :: Lexer Lexeme
small =
  SMALL <$ symbol "SMALL"
    <|> SMALL <$ symbol "TINY"
    <|> SMALL <$ symbol "LITTLE"

floor' :: Lexer Lexeme
floor' =
  FLOOR <$ symbol "FLOOR" <|> FLOOR <$ symbol "GROUND"

get :: Lexer Lexeme
get = GET <$ symbol "GET" <|> GET <$ symbol "TAKE"

close :: Lexer Lexeme
close = CLOSE <$ symbol "CLOSE" <|> CLOSE <$ symbol "SHUT"

go :: Lexer Lexeme
go = GO <$ symbol "GO"

tokens :: Lexer [Lexeme]
tokens = sc *> many term <* Lexer eof

type HasLexeme :: Type -> Constraint
class HasLexeme a where
  toLexeme :: a -> Lexeme
