module Test.Lexer where
import           Data.Either              (isLeft, isRight)
import qualified Data.Text
import           Lexer                    (Lexeme, runParser, tokens)
import           Relude.String.Conversion (toText)
import           Test.Hspec               (describe, hspec, it, shouldBe)
import           Test.Hspec.Runner        (Spec)

lexemes :: [Lexeme]
lexemes = [minBound .. maxBound]

main  :: IO ()
main = hspec spec

checkLexer :: Bool
checkLexer = isRight $ runParser tokens $ Data.Text.unwords $ toText <$> lexemes

spec :: Spec
spec = describe "check lexer" $
  it "lexer parses all tokens"
    $ checkLexer `shouldBe` True
