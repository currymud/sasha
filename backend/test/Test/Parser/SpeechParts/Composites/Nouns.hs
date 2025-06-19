module      Test.Parser.SpeechParts.Composites.Nouns where

import           Data.Text                                (Text)
import           Lexer                                    (runParser, tokens)
import           Lexer.Model                              (Lexeme)
import           Parser.NounParsers                       (objectPathPhraseParser)
import           Parser.PhraseParsers                     (adjPhraseRule)
import           Parser.SpeechParts                       (determinerRule,
                                                           parseRule)
import           Parser.SpeechParts.Atomics.Nouns         (ObjectPath (ObjectPath),
                                                           objectPaths)
import           Parser.SpeechParts.Composites.Adjectives (AdjPhrase)
import           Parser.SpeechParts.Composites.Nouns      (ObjectPathPhrase (..))
import           Prelude                                  hiding (unwords)
import           Relude.String.Conversion                 (ToText (toText))
import           Test.Hspec                               (Spec, hspec)
import           Test.QuickCheck.Arbitrary.Generic        (Arbitrary (arbitrary))
import           Test.QuickCheck.Gen                      (Gen)
import           Text.Earley.Grammar                      (Grammar, Prod)
import           Text.Earley.Parser                       (fullParses, parser)

main :: IO ()
main = hspec spec

checkContainerPhrase :: Gen Bool
checkContainerPhrase = do
--  containerPhrase <- arbitrary :: Gen ContainerPhrase
  pure True

checkObjectPathPhrase :: Gen Bool
checkObjectPathPhrase = do
  objPathPhrase <- arbitrary :: Gen ObjectPathPhrase
  case objPathPhrase of
    SimpleObjectPathPhrase objPath -> case runParser tokens (toText objPath) of
                                        Left _     -> pure False
                                        Right toks ->
                                          pure roundTrip
                                          where
                                            roundTrip =
                                              objPathPhrase `elem` parsed toks
    ObjectPathPhrase det objPath -> pure True
    ObjectPathPhraseAdj _ _ _ -> pure True
  where
    runLexer :: ObjectPath -> Either Text [Lexeme]
    runLexer objPath = runParser tokens (toText objPath)
    objPathRule :: Grammar r (Prod r Text Lexeme ObjectPath)
    objPathRule = parseRule objectPaths ObjectPath
    adjPhraseRule' :: Grammar r (Prod r Text Lexeme AdjPhrase)
    adjPhraseRule' = adjPhraseRule
    objectPathPhraseRule' :: Grammar r (Prod r Text Lexeme ObjectPathPhrase)
    objectPathPhraseRule' = do
      determinerRule' <- determinerRule
      objPathRule' <- objPathRule
      adjPhraseRule' <- adjPhraseRule
      objectPathPhraseParser determinerRule' objPathRule' adjPhraseRule'
    objectPathPhraseParser' = parser objectPathPhraseRule'
    parsed toks =
      fst (fullParses objectPathPhraseParser' toks)


objectPathPhraseRule :: Grammar r (Prod r Text Lexeme ObjectPathPhrase)
objectPathPhraseRule = do
  determinerRule' <- determinerRule
  objPathRule' <- parseRule objectPaths ObjectPath
  adjPhraseRule' <- adjPhraseRule
  objectPathPhraseParser determinerRule' objPathRule' adjPhraseRule'
spec :: Spec
spec = pure ()
