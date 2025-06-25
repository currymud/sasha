module      Test.Parser.SpeechParts.Composites.Prepositions where

import           Data.Text                                  (Text)
import           Lexer                                      (Lexeme)
import           Parser.NounParsers                         (namedAgentRule)
import           Parser.PrepParser                          (containmentMarkerRule,
                                                             instrumentalMarkerRule,
                                                             pathRule,
                                                             recipientMarkerRule,
                                                             targetedStimulusMarkerRule,
                                                             traversalMarkerRule)
import           Parser.SpeechParts.Composites.Nouns        (containerPhraseRule,
                                                             objectPhraseRule,
                                                             targetedStimulusNounPhraseRule)
import           Parser.SpeechParts.Composites.Prepositions (InstrumentMarkerPhrase,
                                                             InstrumentMarkerPhraseRules (..),
                                                             TargetedMarkerPhrase,
                                                             TargetedMarkerPhraseRules (..),
                                                             TraversalPathPhrase,
                                                             TraversalPathPhraseRules (..),
                                                             instrumentMarkerPhraseRules,
                                                             targetedMarkerPhraseRules,
                                                             traversalPathPhraseRule)
import           Prelude                                    hiding (unwords)
import           Relude.String.Conversion                   (ToText (toText))
import           Test.Hspec                                 (Spec, describe,
                                                             hspec)
import           Test.Hspec.QuickCheck                      (prop)
import           Test.Parser.SpeechParts.Composites.Nouns   (containerPhraseRule',
                                                             objectPathPhraseRule,
                                                             objectPhraseRule',
                                                             runLexer,
                                                             targetedStimulusNounPhraseRule')
import           Test.QuickCheck.Arbitrary                  (arbitrary)
import           Test.QuickCheck.Gen                        (Gen)
import           Text.Earley                                (Prod)
import           Text.Earley.Grammar                        (Grammar)
import           Text.Earley.Parser                         (fullParses, parser)

main :: IO ()
main = hspec spec

instrumentMarkerPhraseRules' :: Grammar r (Prod r Text Lexeme InstrumentMarkerPhrase)
instrumentMarkerPhraseRules' = do
  instrumentalMarkerRules' <- instrumentalMarkerRule
  objectPathPhraseRules' <- objectPathPhraseRule
  instrumentMarkerPhraseRules
    $ InstrumentMarkerPhraseRules instrumentalMarkerRules' objectPathPhraseRules'

checkInstrumentMarkerPhrase :: Gen Bool
checkInstrumentMarkerPhrase = do
  instrumentMarkerPhrase' <- arbitrary :: Gen InstrumentMarkerPhrase
  case runLexer (toText instrumentMarkerPhrase') of
   Left _  -> pure False
   Right toks -> pure roundTrip
     where
       roundTrip = instrumentMarkerPhrase' `elem` parsed
       instrumentMarkerPhraseParser' = parser instrumentMarkerPhraseRules'
       parsed = fst (fullParses instrumentMarkerPhraseParser' toks)

targetedMarkerPhraseRule :: Grammar r (Prod r Text Lexeme TargetedMarkerPhrase)
targetedMarkerPhraseRule = do
 targetedStimulusMarkerRule' <- targetedStimulusMarkerRule
 recipientMarkerRule' <- recipientMarkerRule
 targetedStimulusNounPhraseRule'' <- targetedStimulusNounPhraseRule'
 namedAgentRule' <- namedAgentRule
 targetedMarkerPhraseRules
   $ TargetedMarkerPhraseRules
       targetedStimulusMarkerRule'
       recipientMarkerRule'
       targetedStimulusNounPhraseRule''
       namedAgentRule'

checkTargetedMarkerPhrase :: Gen Bool
checkTargetedMarkerPhrase = do
  targetedMarkerPhrase' <- arbitrary :: Gen TargetedMarkerPhrase
  case runLexer (toText targetedMarkerPhrase') of
   Left _  -> pure False
   Right toks -> pure roundTrip
     where
       roundTrip = targetedMarkerPhrase' `elem` parsed
       targetedMarkerPhraseParser' = parser targetedMarkerPhraseRule
       parsed = fst (fullParses targetedMarkerPhraseParser' toks)

traversalPathPhraseRule' :: Grammar r (Prod r Text Lexeme TraversalPathPhrase)
traversalPathPhraseRule' = do
  pathRule' <- pathRule
  traversalMarkerRule' <- traversalMarkerRule
  objectPhraseRule'' <- objectPhraseRule'
  containerMarkerRule' <- containmentMarkerRule
  containerPhraseRule'' <- containerPhraseRule'
  traversalPathPhraseRule
    $ TraversalPathPhraseRules
        pathRule'
        traversalMarkerRule'
        objectPhraseRule''
        containerMarkerRule'
        containerPhraseRule''

checkTraversalPathPhrase :: Gen Bool
checkTraversalPathPhrase = do
  traversalPathPhrase' <- arbitrary :: Gen TraversalPathPhrase
  case runLexer (toText traversalPathPhrase') of
   Left _  -> pure False
   Right toks -> pure roundTrip
     where
       roundTrip = traversalPathPhrase' `elem` parsed
       traversalPathPhraseParser' = parser traversalPathPhraseRule'
       parsed = fst (fullParses traversalPathPhraseParser' toks)

spec :: Spec
spec = describe "Prepositional Phrases roundtrip" $ do
  prop "InstrumentMarkerPhrase roundtrip" checkInstrumentMarkerPhrase
  prop "TargetedMarkerPhrase roundtrip" checkTargetedMarkerPhrase
  prop "TraversalPathPhrase roundtrip" checkTraversalPathPhrase
