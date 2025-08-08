module Grammar.Parser.Partitions.Nouns.Utils (lexemes,objectives', containers', consumables',supportives',surfaces') where

import           Data.List            (nub)
import           Grammar.Parser.Lexer (Lexeme (CHAIR, MAIL, PILL, POCKET, ROBE, SATCHEL, TABLE, TOWEL))

lexemes :: [Lexeme]
lexemes = nub $
  objectives'
    <> containers'
    <> consumables'
    <> supportives'
    <> surfaces'

objectives' :: [Lexeme]
objectives' = [POCKET,TABLE,CHAIR,PILL,MAIL,SATCHEL,TOWEL,ROBE]

containers' :: [Lexeme]
containers' = [POCKET,SATCHEL,ROBE]

supportives' :: [Lexeme]
supportives' = [CHAIR,TABLE,SATCHEL]

surfaces' :: [Lexeme]
surfaces' = [CHAIR,TABLE,SATCHEL]

consumables' :: [Lexeme]
consumables' = [PILL]
