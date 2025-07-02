module      Test.Parser.SpeechParts.Composites.Verbs where

import           Data.Text                                       (Text)
import           Debug.Trace                                     (trace)
import           Lexer                                           (Lexeme)
import           Parser.Model.Nouns                              (NounRules (..))
import           Parser.NounRules                                (namedAgentRule)
import           Parser.PrepParser                               (containmentMarkerRule,
                                                                  instrumentalMarkerRule,
                                                                  pathRule,
                                                                  recipientMarkerRule,
                                                                  sourceMarkerRule,
                                                                  targetedStimulusMarkerRule,
                                                                  traversalMarkerRule)
import           Parser.SpeechParts                              (adjRule,
                                                                  determinerRule)
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
import           Parser.SpeechParts.Composites.Verbs             (AccessVerbPhrase,
                                                                  AcquisitionVerbPhrase,
                                                                  AcquisitionVerbPhraseRules (AcquisitionVerbPhraseRules),
                                                                  GeneralPlacementVerbPhrase,
                                                                  GeneralPlacementVerbPhraseRules (GeneralPlacementVerbPhraseRules),
                                                                  Imperative,
                                                                  StimulusVerbPhrase,
                                                                  TraversalVerbPhrase,
                                                                  TraversalVerbPhraseRules (..),
                                                                  acquisitionVerbPhraseRule,
                                                                  generalPlacementVerbPhraseRule,
                                                                  traversalVerbPhraseRule)
import           Parser.VerbParsers                              (accessVerbPhraseRules,
                                                                  acquisitionVerbRule,
                                                                  generalPlacementVerbRule,
                                                                  imperativeRules,
                                                                  stimulusVerbPhraseRules,
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
  case runLexer (toText acquisitionVerbPhrase) of
    Left _  -> pure False
    Right toks -> pure roundTrip
      where
        roundTrip = acquisitionVerbPhrase `elem` parsed
        acquisitionVerbPhraseParser' = parser acquisitionVerbPhraseRules'
        parsed = fst (fullParses acquisitionVerbPhraseParser' toks)

accessVerbPhraseRules' :: Grammar r (Prod r Text Lexeme AccessVerbPhrase)
accessVerbPhraseRules' = do
  determinerRule' <- determinerRule
  adjectiveRule' <- adjRule
  accessVerbPhraseRules determinerRule' adjectiveRule'

checkAccessVerbPhrase :: Gen Bool
checkAccessVerbPhrase = do
  accessVerbPhrase <- arbitrary :: Gen AccessVerbPhrase
  case runLexer (toText accessVerbPhrase) of
    Left _  -> pure False
    Right toks -> pure roundTrip
      where
        roundTrip = accessVerbPhrase `elem` parsed
        accessVerbPhraseParser' = parser accessVerbPhraseRules'
        parsed = fst (fullParses accessVerbPhraseParser' toks)

stimulusVerbPhraseRules' :: Grammar r (Prod r Text Lexeme StimulusVerbPhrase)
stimulusVerbPhraseRules' = do
  determinerRule' <- determinerRule
  adjectiveRule' <- adjRule
  targetedStimulusNounPhraseRule'' <- targetedStimulusNounPhraseRule'
  containerPhraseRule'' <- containerPhraseRule'
  supportPhraseRule'' <- supportPhraseRule'
  objectPhraseRule'' <- objectPhraseRule'
  let nounParserRules = NounRules
                           targetedStimulusNounPhraseRule''
                           containerPhraseRule''
                           supportPhraseRule''
                           objectPhraseRule''
  stimulusVerbPhraseRules nounParserRules determinerRule' adjectiveRule'

checkStimulusVerbPhrase :: Gen Bool
checkStimulusVerbPhrase = do
  stimulusVerbPhrase <- arbitrary :: Gen StimulusVerbPhrase
  case runLexer (toText stimulusVerbPhrase) of
    Left _  -> pure False
    Right toks -> pure roundTrip
      where
        roundTrip = stimulusVerbPhrase `elem` parsed
        stimulusVerbPhraseParser' = parser stimulusVerbPhraseRules'
        parsed = fst (fullParses stimulusVerbPhraseParser' toks)

imperativeRules' :: Grammar r (Prod r Text Lexeme Imperative)
imperativeRules' = do
  determinerRule' <- determinerRule
  adjectiveRule' <- adjRule
  objectPhraseRule'' <- objectPhraseRule'
  instrumentMarkerPhraseRules' <- instrumentMarkerPhraseRules
  targetedMarkerPhraseRules' <- targetedMarkerPhraseRules
  traversalPathPhraseRules' <- traversalPathPhraseRule
  namedAgentRule' <- namedAgentRule
  imperativeRules $ NounRules
    objectPhraseRule''
    instrumentMarkerPhraseRules'
    targetedMarkerPhraseRules'
    traversalPathPhraseRules'
spec :: Spec
spec = describe "Verb Phrases roundtrip" $ do
  prop "traversalVerbPhraseRule" checkTraversalVerbPhrase
  prop "generalPlacementVerbPhraseRule" checkGeneralPlacementVerbPhrase
  prop "acquisitionVerbPhraseRule" checkAcquisitionVerbPhrase
  prop "accessVerbPhraseRule" checkAccessVerbPhrase
  prop "stimulusVerbPhraseRule" checkStimulusVerbPhrase
