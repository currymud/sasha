module Grammar.Parser.Partitions.Nouns.Utils (lexemes) where

import           Data.HashSet                                (HashSet, toList)
import           Grammar.Parser.Lexer                        (HasLexeme, Lexeme,
                                                              toLexeme)
import           Grammar.Parser.Partitions.Nouns.Containers  (containers)
import           Grammar.Parser.Partitions.Nouns.Objectives  (objectives)
import           Grammar.Parser.Partitions.Nouns.Supportives (supportives)


lexemes :: [Lexeme]
lexemes =
  makeLexemes objectives
  <> makeLexemes containers
  <> makeLexemes supportives

makeLexemes :: (HasLexeme a) => HashSet a -> [Lexeme]
makeLexemes speechParts = toLexeme <$> toList speechParts

