module Grammar.Parser.Partitions.Adjectives (adjectives) where
import           Data.HashSet                    (HashSet, fromList)
import           Model.Parser.Atomics.Adjectives (Adjective (Adjective))
import           Model.Parser.Lexer              (Lexeme (PLANT, POT))

adjectives :: HashSet Adjective
adjectives = fromList $ map Adjective adjlex
  where adjlex = [PLANT, POT]

