module Parser.Partitions.Adverbs where

import           Data.Hashable            (Hashable)
import           Data.HashSet             (HashSet, fromList, singleton)
import           Data.Kind                (Type)
import           Lexer
import           Relude.String.Conversion (ToText)

rotationalDirections :: HashSet RotationalDirection
rotationalDirections =
  fromList $ map RotationalDirection [LEFT, RIGHT, CLOCKWISE, COUNTERCLOCKWISE]

implicitPaths :: HashSet ImplicitPath
implicitPaths = fromList $ map ImplicitPath [NORTH, EAST, SOUTH, WEST, UP, DOWN, LEFT,RIGHT]

modToggleAdverbs :: HashSet ModToggleAdverb
modToggleAdverbs = fromList $ map ModToggleAdverb [ON, OFF]

researchAdverbs :: HashSet ResearchAdverb
researchAdverbs = singleton $ ResearchAdverb UP
