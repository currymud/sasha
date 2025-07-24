module Grammar.Parser.Partitions.Misc where
import           Data.HashSet              (HashSet, fromList)
import           Model.Parser.Atomics.Misc (Determiner (Determiner))
import           Model.Parser.Lexer        (Lexeme (A, THE))

determiners :: HashSet Determiner
determiners = fromList $ map Determiner [THE, A]

