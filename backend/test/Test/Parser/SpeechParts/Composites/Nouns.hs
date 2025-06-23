module      Test.Parser.SpeechParts.Composites.Nouns where

import           Data.Text                               (Text, unwords)
import           Debug.Trace                             (trace)
import           Lexer                                   (runParser, tokens)
import           Lexer.Model                             (Lexeme)
import           Parser                                  (Parser)
import           Parser.NounParsers                      (containerRule,
                                                          directionalStimulusNounRule,
                                                          objectPathPhraseParser,
                                                          objectivePhraseParser,
                                                          surfaceRule)
import           Parser.PhraseParsers                    (adjPhraseRule,
                                                          implicitPathRule)
import           Parser.PrepParser                       (containmentMarkerRule,
                                                          pathRule,
                                                          surfaceMarkerRule)
import           Parser.SpeechParts                      (determinerRule,
                                                          parseRule)
import           Parser.SpeechParts.Atomics.Nouns        (ObjectPath (ObjectPath),
                                                          objectPaths)
import           Parser.SpeechParts.Atomics.Prepositions (InstrumentalMarker (InstrumentalMarker),
                                                          instrumentalMarker)
import           Parser.SpeechParts.Composites.Nouns     (ContainerPhrase (..),
                                                          ContainerPhraseRules (..),
                                                          DirectionalStimulusNounPhrase (..),
                                                          DirectionalStimulusNounRules (..),
                                                          NounPhrase (..),
                                                          ObjectPathPhrase (..),
                                                          ObjectPhrase (..),
                                                          PathPhrase (..),
                                                          PathPhraseRules (..),
                                                          PrepObjectPhrase (Instrument),
                                                          PrepObjectPhraseRules (PrepObjectPhraseRules),
                                                          SupportPhrase (..),
                                                          SupportPhraseRules (..),
                                                          SurfacePhrase (..),
                                                          SurfacePhraseRules (..),
                                                          containerPhraseRule,
                                                          directionalStimulusNounPhraseRule,
                                                          pathPhraseRule,
                                                          prepObjectPhraseRule,
                                                          supportPhraseRule,
                                                          surfacePhraseRule)
import           Prelude                                 hiding (unwords)
import           Relude.String.Conversion                (ToText (toText))
import           Test.Hspec                              (Spec, describe, hspec)
import           Test.Hspec.QuickCheck                   (prop)
import           Test.QuickCheck.Arbitrary.Generic       (Arbitrary (arbitrary))
import           Test.QuickCheck.Gen                     (Gen)
import           Text.Earley.Grammar                     (Grammar, Prod)
import           Text.Earley.Parser                      (fullParses, parser)

main :: IO ()
main = hspec spec


checkObjectPathPhrase :: Gen Bool
checkObjectPathPhrase = do
  objPathPhrase <- arbitrary :: Gen ObjectPathPhrase
  case objPathPhrase of
    SimpleObjectPathPhrase objPath -> case runLexer (toText objPath) of
      Left _     -> pure False
      Right toks -> pure roundTrip
                    where
                      roundTrip =
                        objPathPhrase `elem` parsed toks
    ObjectPathPhrase det objPath -> case runLexer textify of
      Left _     -> pure False
      Right toks -> pure roundTrip
                    where
                      roundTrip =
                        objPathPhrase `elem` parsed toks
      where
        textify = unwords [toText det, toText objPath]
    ObjectPathPhraseAdj det adjPhrase objPath -> case runLexer textify of
      Left _     -> pure False
      Right toks -> pure roundTrip
                    where
                      roundTrip =
                        objPathPhrase `elem` parsed toks
      where
        textify = unwords [toText det,toText adjPhrase, toText objPath]
  where
    objectPathPhraseParser' = parser objectPathPhraseRule
    parsed toks =
      fst (fullParses objectPathPhraseParser' toks)

runLexer :: Text -> Either Text [Lexeme]
runLexer = runParser tokens

objectPathPhraseRule :: Grammar r (Prod r Text Lexeme ObjectPathPhrase)
objectPathPhraseRule = do
  determinerRule' <- determinerRule
  objPathRule' <- parseRule objectPaths ObjectPath
  adjPhraseRule' <- adjPhraseRule
  objectPathPhraseParser determinerRule' objPathRule' adjPhraseRule'

checkPathPhrase :: Gen Bool
checkPathPhrase = do
  pathPhrase <- arbitrary :: Gen PathPhrase
  case pathPhrase of
    SimplePath implicitPath -> case runLexer (toText implicitPath) of
      Left _     -> trace "failed simplePath" pure False
      Right toks -> trace ("simplePath result" <> show toks) $ pure roundTrip
                    where
                      roundTrip =
                        pathPhrase `elem` parsed toks
    PathPhrase path det objPath -> case runLexer textify of
      Left _     -> trace "failed pathPhras" $ pure False
      Right toks -> trace ("pathPhrase result" <> show toks) $ pure roundTrip
                    where
                      roundTrip =
                        pathPhrase `elem` parsed toks
      where
        textify = unwords [toText path,toText det, toText objPath]
  where
    parsed toks =
      fst (fullParses pathPhraseParser' toks)
    pathPhraseParser' = parser pathPhraseRule'

pathPhraseRule' :: Grammar r (Prod r Text Lexeme PathPhrase)
pathPhraseRule' = do
  implicitPathRule' <- implicitPathRule
  objPathRule' <- parseRule objectPaths ObjectPath
  pathRule' <- pathRule
  determinerRule' <- determinerRule
  pathPhraseRule
    $ PathPhraseRules pathRule' implicitPathRule' determinerRule' objPathRule'

checkPrepObjectPhrase :: Gen Bool
checkPrepObjectPhrase = do
  (Instrument instrumentMarker objectPathPhrase) <- arbitrary :: Gen PrepObjectPhrase
  let textify = unwords [toText instrumentMarker, toText objectPathPhrase]
  case runLexer textify of
    Left _     -> pure False
    Right toks -> pure roundTrip
      where
        roundTrip =
          Instrument instrumentMarker objectPathPhrase `elem` parsed toks
  where
    prepObjectPhraseParser' = parser prepObjectPhraseRule'
    parsed toks =
      fst (fullParses prepObjectPhraseParser' toks)

prepObjectPhraseRule' :: Grammar r (Prod r Text Lexeme PrepObjectPhrase)
prepObjectPhraseRule' = do
  instrumentMarker <- parseRule instrumentalMarker InstrumentalMarker
  objPathPhrase <- objectPathPhraseRule
  prepObjectPhraseRule $ PrepObjectPhraseRules instrumentMarker objPathPhrase

checkObjectPhrase :: Gen Bool
checkObjectPhrase = do
  (ObjectPhrase nounPhrase) <- arbitrary :: Gen ObjectPhrase
  case nounPhrase of
    SimpleNounPhrase noun -> case runLexer (toText noun) of
      Left _     -> pure False
      Right toks -> pure roundTrip
                    where
                      roundTrip =
                        ObjectPhrase nounPhrase `elem` parsed toks
    NounPhrase det noun -> case runLexer textify of
      Left _     -> pure False
      Right toks -> pure roundTrip
                    where
                      roundTrip =
                        ObjectPhrase nounPhrase `elem` parsed toks
      where
        textify = unwords [toText det, toText noun]
    DescriptiveNounPhrase adjPhrase noun -> case runLexer textify of
      Left _     -> pure False
      Right toks -> pure roundTrip
                    where
                      roundTrip =
                        ObjectPhrase nounPhrase `elem` parsed toks
      where
        textify = unwords [toText adjPhrase, toText noun]
    DescriptiveNounPhraseDet det adjPhrase noun -> case runLexer textify of
      Left _     -> pure False
      Right toks -> pure roundTrip
                    where
                      roundTrip =
                        ObjectPhrase nounPhrase `elem` parsed toks
      where
        textify = unwords [toText det, toText adjPhrase, toText noun]

  where
    objectPhraseParser' = parser objectPhraseRule'
    parsed toks =
      fst (fullParses objectPhraseParser' toks)

objectPhraseRule':: Grammar r (Prod r Text Lexeme ObjectPhrase)
objectPhraseRule' = do
  determinerRule' <- determinerRule
  adjPhraseRule' <- adjPhraseRule
  objectivePhraseParser determinerRule' adjPhraseRule'

checkSurfacePhrase :: Gen Bool
checkSurfacePhrase = do
  surfacePhrase <- arbitrary :: Gen SurfacePhrase
  case surfacePhrase of
    SimpleSurfacePhrase nounPhrase -> case runLexer (toText nounPhrase) of
      Left _     -> pure False
      Right toks -> pure roundTrip
                    where
                      roundTrip =
                        surfacePhrase `elem` parsed toks
    surfacePhrase' -> case runLexer (toText surfacePhrase') of
      Left _     -> pure False
      Right toks -> pure roundTrip
                    where
                      roundTrip =
                        surfacePhrase `elem` parsed toks
  where
    surfacePhraseParser' = parser surfacePhraseRule'
    parsed toks = fst (fullParses surfacePhraseParser' toks)

surfacePhraseRule' :: Grammar r (Prod r Text Lexeme SurfacePhrase)
surfacePhraseRule' = do
  determinerRule' <- determinerRule
  adjPhraseRule' <- adjPhraseRule
  surfaceRule' <-  surfaceRule
  surfaceMarkerRule' <- surfaceMarkerRule
  surfacePhraseRule $
   SurfacePhraseRules
     determinerRule'
     adjPhraseRule'
     surfaceRule'
     surfaceMarkerRule'

checkContainerPhrase :: Gen Bool
checkContainerPhrase = do
  containerPhrase <- arbitrary :: Gen ContainerPhrase
  trace ("ContainerPhrase: " <> show (toText containerPhrase)) $ do
    case containerPhrase of
      cphrase@(SimpleContainerPhrase {}) -> case runLexer (toText cphrase) of
        Left _     -> pure False
        Right toks -> pure roundTrip
                      where
                        roundTrip =
                          containerPhrase `elem` parsed toks
      cphrase@(ContainerPhrase {}) -> case runLexer (toText cphrase) of
        Left _     -> pure False
        Right toks -> pure roundTrip
                      where
                        roundTrip =
                          containerPhrase `elem` parsed toks
    where
      containerPhraseParser' = parser containerPhraseRule'
      parsed toks =
        fst (fullParses containerPhraseParser' toks)

containerPhraseRule' :: Grammar r (Prod r Text Lexeme ContainerPhrase)
containerPhraseRule' = do
  determinerRule' <- determinerRule
  adjPhraseRule' <- adjPhraseRule
  containerRule' <- containerRule
  containmentMarkerRule' <- containmentMarkerRule
  containerPhraseRule $
    ContainerPhraseRules
      determinerRule'
      adjPhraseRule'
      containerRule'
      containmentMarkerRule'

supportPhraseRule' :: Grammar r (Prod r Text Lexeme SupportPhrase)
supportPhraseRule' = do
  surfacePhraseRule'' <- surfacePhraseRule'
  containerPhraseRule'' <- containerPhraseRule'
  supportPhraseRule $ SupportPhraseRules surfacePhraseRule'' containerPhraseRule''

checkSupportPhrase :: Gen Bool
checkSupportPhrase = do
  supportPhrase <- arbitrary :: Gen SupportPhrase
  case supportPhrase of
    surfacePhrase@(SurfaceSupport {}) -> case runLexer (toText surfacePhrase) of
      Left _     -> pure False
      Right toks -> pure roundTrip
                    where
                      roundTrip =
                        supportPhrase `elem` parsed toks
    containerPhrase@(ContainerSupport {}) -> case runLexer (toText containerPhrase) of
      Left _     -> pure False
      Right toks -> pure roundTrip
                    where
                      roundTrip =
                        supportPhrase `elem` parsed toks
  where
    supportPhraseParser' = parser supportPhraseRule'
    parsed toks =
      fst (fullParses supportPhraseParser' toks)

checkDirectionalStimulusNounPhrase :: Gen Bool
checkDirectionalStimulusNounPhrase = do
  directionalStimulusNounPhrase <- arbitrary :: Gen DirectionalStimulusNounPhrase
  case directionalStimulusNounPhrase of
    directionalStimulusNounPhrase'@(DirectionalStimulusNounPhrase {}) -> case runLexer (toText directionalStimulusNounPhrase') of
      Left _     -> pure False
      Right toks -> pure roundTrip
                    where
                      roundTrip =
                        directionalStimulusNounPhrase `elem` parsed toks
  where
    directionalStimulusNounPhraseParser' = parser directionalStimulusNounPhraseRule'
    parsed toks =
      fst (fullParses directionalStimulusNounPhraseParser' toks)

directionalStimulusNounPhraseRule' :: Grammar r (Prod r Text Lexeme DirectionalStimulusNounPhrase)
directionalStimulusNounPhraseRule' = do
  determinerRule' <- determinerRule
  adjPhraseRule' <- adjPhraseRule
  directionalStimulusNounRule' <- directionalStimulusNounRule
  directionalStimulusNounPhraseRule
    $ DirectionalStimulusNounRules
        determinerRule'
        adjPhraseRule'
        directionalStimulusNounRule'
spec :: Spec
spec = describe "NounPhrase Roundtrips" do
  prop "ObjPathPhrase round tripping" checkObjectPathPhrase
  prop "PathPhrase round tripping" checkPathPhrase
  prop "PrepObjectPhrase round tripping" checkPrepObjectPhrase
  prop "ObjectPhrase round tripping" checkObjectPhrase
  prop "SurfacePhrase round tripping" checkSurfacePhrase
  prop "ContainerPhrase round tripping" checkContainerPhrase
  prop "SupportPhrase round tripping" checkSupportPhrase
  prop "DirectionalStimulusNounPhrase round tripping" checkDirectionalStimulusNounPhrase
