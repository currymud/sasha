module Test.Lexer where
import           Data.Either              (isRight)
import qualified Data.Text
import           Lexer                    (Lexeme, lexify, tokens)
import           Relude.String.Conversion (toText)
import           Test.Hspec               (describe, hspec, it, shouldBe)
import           Test.Hspec.Runner        (Spec)

lexemes :: [Lexeme]
lexemes = [minBound .. maxBound]

main  :: IO ()
main = hspec spec

spec :: Spec
spec = describe "check lexer" $
  it "lexer parses all tokens"
    $ checkLexer `shouldBe` True

checkLexer :: Bool
checkLexer = isRight $ lexify tokens $ Data.Text.unwords $ toText <$> lexemes
