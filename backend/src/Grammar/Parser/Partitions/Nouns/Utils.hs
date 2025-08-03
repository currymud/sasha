module Grammar.Parser.Partitions.Nouns.Utils (lexemes,objectives', containers', edibles',supportives',surfaces') where

import           Data.List            (nub)
import           Grammar.Parser.Lexer (Lexeme (CHAIR, MAIL, PILL, ROBE, SATCHEL, TABLE, TOWEL))

lexemes :: [Lexeme]
lexemes = nub $
  objectives'
    <> containers'
    <> edibles'
    <> supportives'
    <> surfaces'

objectives' :: [Lexeme]
objectives' = [TABLE,CHAIR,PILL,MAIL,SATCHEL,TOWEL,ROBE]

containers' :: [Lexeme]
containers' = [SATCHEL,ROBE]

supportives' :: [Lexeme]
supportives' = [CHAIR,TABLE,SATCHEL]

surfaces' :: [Lexeme]
surfaces' = [CHAIR,TABLE,SATCHEL]

edibles' :: [Lexeme]
edibles' = [PILL]
