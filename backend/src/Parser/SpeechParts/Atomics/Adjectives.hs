module Parser.SpeechParts.Atomics.Adjectives where

import           Data.Hashable (Hashable)
import           Data.HashSet  (HashSet, fromList)
import           Data.Kind     (Type)
import           Lexer

type Adjective :: Type
newtype Adjective =
  Adjective { _fromAdjective :: Lexeme } deriving newtype (Show,Eq,Hashable,Ord)

instance HasLexeme Adjective where
  toLexeme = _fromAdjective

adjectives :: HashSet Adjective
adjectives = fromList $ map Adjective adjlex
  where adjlex = [MIND, BLUE, RED, GREAT, LONG, OLD, DRUNK, PLANT, POT, TEA
                 , CABINET, LOCKED, UNLOCKED, KITCHEN, LEFT, RIGHT
                 , FRONT, BEHIND, SMALL, LARGE]
