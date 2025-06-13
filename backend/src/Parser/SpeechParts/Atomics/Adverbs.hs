module Parser.SpeechParts.Atomics.Adverbs where

import           Data.Hashable (Hashable)
import           Data.HashSet  (HashSet, fromList, singleton)
import           Data.Kind     (Type)
import           Lexer

type RotationalDirection :: Type
newtype RotationalDirection =
  RotationalDirection { _fromRotationalDirection :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme RotationalDirection where
  toLexeme = _fromRotationalDirection

rotationalDirections :: HashSet RotationalDirection
rotationalDirections =
  fromList $ map RotationalDirection [LEFT, RIGHT, CLOCKWISE, COUNTERCLOCKWISE]

type ImplicitPath :: Type
newtype ImplicitPath = ImplicitPath { _fromImplicitPath :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme ImplicitPath where
  toLexeme = _fromImplicitPath

implicitPaths :: HashSet ImplicitPath
implicitPaths = fromList $ map ImplicitPath [NORTH, EAST, SOUTH, WEST, UP, DOWN, LEFT,RIGHT]

type ModToggleAdverb :: Type
newtype ModToggleAdverb = ModToggleAdverb { _fromModToggleAdverb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme ModToggleAdverb where
  toLexeme = _fromModToggleAdverb

modToggleAdverbs :: HashSet ModToggleAdverb
modToggleAdverbs = fromList $ map ModToggleAdverb [ON, OFF]

type ResearchAdverb :: Type
newtype ResearchAdverb = ResearchAdverb { _fromResearchAdverb :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme ResearchAdverb where
  toLexeme = _fromResearchAdverb

researchAdverbs :: HashSet ResearchAdverb
researchAdverbs = singleton $ ResearchAdverb UP
