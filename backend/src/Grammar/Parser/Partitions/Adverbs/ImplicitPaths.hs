module Grammar.Parser.Partitions.Adverbs.ImplicitPaths ( implicitPaths
                                                       ,  north
                                                       ,  east
                                                       ,  south
                                                       ,  west
                                                       ,  up
                                                       ,  down
                                                       ,  left
                                                       ,  right) where
import           Data.HashSet                                (HashSet, fromList)
import           Grammar.Parser.Lexer                        (Lexeme (DOWN, EAST, LEFT, NORTH, RIGHT, SOUTH, UP, WEST))
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Adverbs                (ImplicitPath (ImplicitPath))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Test.QuickCheck.Arbitrary                   (Arbitrary (Arbitrary, arbitrary),
                                                              elements)
#endif

makeSemanticValues [| ImplicitPath |] [NORTH, EAST, SOUTH, WEST, UP, DOWN, LEFT,RIGHT]

implicitPaths :: HashSet ImplicitPath
implicitPaths =
  fromList [north,east,south,west,up,down,left,right]

#ifdef TESTING

instance Arbitrary ImplicitPath where
  arbitrary = elements $ HS.toList implicitPaths
#endif
