module      Test.Parser.SpeechParts.Composites.Verbs where

import           Data.Text                                       (Text)
import           Debug.Trace                                     (trace)
import           Lexer                                           (Lexeme)
import           Parser.NounParsers                              (namedAgentRule)
import           Parser.PrepParser                               (containmentMarkerRule,
                                                                  instrumentalMarkerRule,
                                                                  pathRule,
                                                                  recipientMarkerRule,
                                                                  sourceMarkerRule,
                                                                  targetedStimulusMarkerRule,
                                                                  traversalMarkerRule)
import           Parser.SpeechParts.Atomics.Verbs                (GeneralPlacementVerb)
import           Parser.SpeechParts.Composites.Nouns             (containerPhraseRule,
                                                                  objectPhraseRule,
                                                                  targetedStimulusNounPhraseRule)
import           Parser.SpeechParts.Composites.Prepositions      (InstrumentMarkerPhrase,
                                                                  InstrumentMarkerPhraseRules (..),
                                                                  TargetedMarkerPhrase,
                                                                  TargetedMarkerPhraseRules (..),
                                                                  TraversalPathPhrase,
                                                                  TraversalPathPhraseRules (..),
                                                                  instrumentMarkerPhraseRules,
                                                                  targetedMarkerPhraseRules,
                                                                  traversalPathPhraseRule)
import           Parser.SpeechParts.Composites.Verbs             (AcquisitionVerbPhrase,
                                                                  AcquisitionVerbPhraseRules (AcquisitionVerbPhraseRules),
                                                                  GeneralPlacementVerbPhrase,
                                                                  GeneralPlacementVerbPhraseRules (GeneralPlacementVerbPhraseRules),
                                                                  TraversalVerbPhrase,
                                                                  TraversalVerbPhraseRules (..),
                                                                  acquisitionVerbPhraseRule,
                                                                  generalPlacementVerbPhraseRule,
                                                                  traversalVerbPhraseRule)
import           Parser.VerbParsers                              (acquisitionVerbRule,
                                                                  generalPlacementVerbRule,
                                                                  traversalVerbRule)
import           Prelude                                         hiding
                                                                 (unwords)
import           Relude.String.Conversion                        (ToText (toText))
import           Test.Hspec                                      (Spec,
                                                                  describe,
                                                                  hspec)
import           Test.Hspec.QuickCheck                           (prop)
import           Test.Parser.SpeechParts.Composites.Nouns        (containerPhraseRule',
                                                                  objectPathPhraseRule,
                                                                  objectPhraseRule',
                                                                  runLexer,
                                                                  supportPhraseRule',
                                                                  targetedStimulusNounPhraseRule')
import           Test.Parser.SpeechParts.Composites.Prepositions (traversalPathPhraseRule')
import           Test.QuickCheck.Arbitrary                       (arbitrary)
import           Test.QuickCheck.Gen                             (Gen)
import           Text.Earley                                     (Prod)
import           Text.Earley.Grammar                             (Grammar)
import           Text.Earley.Parser                              (fullParses,
                                                                  parser)

main :: IO ()
main = hspec spec

traversalVerbPhraseRules' :: Grammar r (Prod r Text Lexeme TraversalVerbPhrase)
traversalVerbPhraseRules' = do
  traversalVerbRule' <- traversalVerbRule
  objectPhraseRule'' <- objectPhraseRule'
  traversalPathPhraseRule'' <- traversalPathPhraseRule'
  traversalVerbPhraseRule
    $ TraversalVerbPhraseRules
        traversalVerbRule'
        objectPhraseRule''
        traversalPathPhraseRule''

checkTraversalVerbPhrase :: Gen Bool
checkTraversalVerbPhrase = do
  traversalVerbPhrase <- arbitrary :: Gen TraversalVerbPhrase
  case runLexer (toText traversalVerbPhrase) of
    Left _  -> pure False
    Right toks -> pure roundTrip
      where
        roundTrip = traversalVerbPhrase `elem` parsed
        traversalVerbPhraseParser' = parser traversalVerbPhraseRules'
        parsed = fst (fullParses traversalVerbPhraseParser' toks)

generalPlacementVerbPhraseRules' :: Grammar r (Prod r Text Lexeme GeneralPlacementVerbPhrase)
generalPlacementVerbPhraseRules' = do
  generalPlacementVerbRule' <- generalPlacementVerbRule
  objectPhraseRule'' <- objectPhraseRule'
  supportPhraseRule'' <- supportPhraseRule'
  generalPlacementVerbPhraseRule
    $ GeneralPlacementVerbPhraseRules
        generalPlacementVerbRule'
        objectPhraseRule''
        supportPhraseRule''

checkGeneralPlacementVerbPhrase :: Gen Bool
checkGeneralPlacementVerbPhrase = do
  generalPlacementVerbPhrase <- arbitrary :: Gen GeneralPlacementVerbPhrase
  case runLexer (toText generalPlacementVerbPhrase) of
    Left _  -> pure False
    Right toks -> pure roundTrip
      where
        roundTrip = generalPlacementVerbPhrase `elem` parsed
        generalPlacementVerbPhraseParser' = parser generalPlacementVerbPhraseRules'
        parsed = fst (fullParses generalPlacementVerbPhraseParser' toks)

acquisitionVerbPhraseRules' :: Grammar r (Prod r Text Lexeme AcquisitionVerbPhrase)
acquisitionVerbPhraseRules' = do
  acquisitionVerbRule' <- acquisitionVerbRule
  objectPhraseRule'' <- objectPhraseRule'
  sourceMarkerRule' <- sourceMarkerRule
  supportPhraseRule'' <- supportPhraseRule'
  acquisitionVerbPhraseRule
    $ AcquisitionVerbPhraseRules
        acquisitionVerbRule'
        objectPhraseRule''
        sourceMarkerRule'
        supportPhraseRule''

checkAcquisitionVerbPhrase :: Gen Bool
checkAcquisitionVerbPhrase = do
  acquisitionVerbPhrase <- arbitrary :: Gen AcquisitionVerbPhrase
  trace ("***PRIOR***" <> show acquisitionVerbPhrase) $ do
    case runLexer (toText acquisitionVerbPhrase) of
      Left _  -> trace "failed acquisitoion verb" $ pure False
      Right toks -> trace ("parsed " <> show parsed <> " " <> "AVP " <> show parsed) $ pure roundTrip
        where
          roundTrip = acquisitionVerbPhrase `elem` parsed
          acquisitionVerbPhraseParser' = parser acquisitionVerbPhraseRules'
          parsed = fst (fullParses acquisitionVerbPhraseParser' toks)

spec :: Spec
spec = describe "Verb Phrases roundtrip" $ do
  prop "traversalVerbPhraseRule" checkTraversalVerbPhrase
  prop "generalPlacementVerbPhraseRule" checkGeneralPlacementVerbPhrase
  prop "acquisitionVerbPhraseRule" checkAcquisitionVerbPhrase
