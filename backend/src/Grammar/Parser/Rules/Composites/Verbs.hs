module Grammar.Parser.Rules.Composites.Verbs
  (stimulusVerbPhraseRules
  , imperativeRules
  ) where
import           Data.Text                                               (Text)
import           GHC.Base                                                (Alternative ((<|>)))
import           Grammar.Parser.Lexer                                    (Lexeme)
import           Grammar.Parser.Partitions.Adjectives                    (adjectives)
import           Grammar.Parser.Partitions.Misc                          (determiners)
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus     (directionalStimulii)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (directionalStimulusVerbs)
import           Grammar.Parser.Rules.Atomics.Prepositions               (directionalStimulusMarkerRule)
import           Grammar.Parser.Rules.Atomics.Utils                      (parseRule)
import           Grammar.Parser.Rules.Atomics.Verbs                      (implicitStimulusVerbRule)
import           Grammar.Parser.Rules.Composites.Nouns                   (directionalStimulusNounPhraseRules)
import           Model.Parser.Atomics.Adjectives                         (Adjective (Adjective))
import           Model.Parser.Atomics.Misc                               (Determiner (Determiner))
import           Model.Parser.Atomics.Nouns                              (DirectionalStimulus (DirectionalStimulus))
import           Model.Parser.Atomics.Verbs                              (DirectionalStimulusVerb (DirectionalStimulusVerb))
import           Model.Parser.Composites.Verbs                           (Imperative (StimulusVerbPhrase),
                                                                          StimulusVerbPhrase (DirectStimulusVerbPhrase, ImplicitStimulusVerb))
import           Text.Earley.Grammar                                     (Grammar,
                                                                          Prod,
                                                                          rule)

stimulusVerbPhraseRules :: Grammar r (Prod r Text Lexeme StimulusVerbPhrase)
stimulusVerbPhraseRules = do
  implicitStimulusVerb <- implicitStimulusVerbRule
  directionalStimulusMarker <- directionalStimulusMarkerRule
  directionalStimulusVerb <- parseRule directionalStimulusVerbs DirectionalStimulusVerb
  directionalStimulusNoun <- parseRule directionalStimulii DirectionalStimulus
  determiner <- parseRule determiners Determiner
  adj <- parseRule adjectives Adjective
  directionalStimulusNounPhrase <- directionalStimulusNounPhraseRules determiner adj directionalStimulusNoun
  rule $ ImplicitStimulusVerb <$> implicitStimulusVerb
           <|> DirectStimulusVerbPhrase
             <$> directionalStimulusVerb
             <*> directionalStimulusMarker
             <*> directionalStimulusNounPhrase

imperativeRules :: Grammar r (Prod r Text Lexeme Imperative)
imperativeRules = do
  stimulusVerbPhrase <- stimulusVerbPhraseRules
  rule $ StimulusVerbPhrase <$> stimulusVerbPhrase
