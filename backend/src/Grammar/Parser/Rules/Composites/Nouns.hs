module Grammar.Parser.Rules.Composites.Nouns (containerPhraseRules,
                                              directionalStimulusNounPhraseRules,
                                              consumableNounPhraseRules,
                                              nounPhraseRule,
                                              objectPhraseRules,
                                              somaticStimulusNounPhraseRules,
                                              surfacePhraseRules,
                                              supportPhraseRules) where
import           Control.Applicative                                   (Alternative ((<|>)))
import           Data.Text                                             (Text)
import           Grammar.Parser.Partitions.Nouns.Containers            (containers)
import           Grammar.Parser.Partitions.Nouns.Surfaces              (surfaces)
import           Grammar.Parser.Partitions.Prepositions                (containmentMarkers)
import           Grammar.Parser.Partitions.Prepositions.SurfaceMarkers (surfaceMarkers)
import           Grammar.Parser.Rules.Atomics.Utils                    (parseRule)
import           Model.Parser.Atomics.Adjectives                       (Adjective)
import           Model.Parser.Atomics.Misc                             (Determiner)
import           Model.Parser.Atomics.Nouns                            (Consumable (Consumable),
                                                                        Container (Container),
                                                                        DirectionalStimulus,
                                                                        Objective,
                                                                        SomaticStimulus,
                                                                        Surface (Surface))
import           Model.Parser.Atomics.Prepositions                     (ContainmentMarker (ContainmentMarker),
                                                                        DirectionalStimulusMarker,
                                                                        SurfaceMarker (SurfaceMarker))
import           Model.Parser.Composites.Nouns                         (ConsumableNounPhrase (ConsumableNounPhrase),
                                                                        ContainerPhrase (ContainerPhrase),
                                                                        DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                                        NounPhrase (DescriptiveNounPhrase, DescriptiveNounPhraseDet, NounPhrase, SimpleNounPhrase),
                                                                        NounPhraseRules (NounPhraseRules, _adjRule, _determinerRule, _nounRule),
                                                                        ObjectPhrase (ObjectPhrase),
                                                                        SomaticStimulusNounPhrase (SomaticStimulusNounPhrase),
                                                                        SupportPhrase (ContainerSupport, SurfaceSupport),
                                                                        SurfacePhrase (SimpleSurfacePhrase, SurfacePhrase))
import           Model.Parser.Lexer                                    (Lexeme)
import           Text.Earley                                           (Grammar)
import           Text.Earley.Grammar                                   (Prod,
                                                                        rule)

containerPhraseRules :: Prod r Text Lexeme Determiner
                                       -> Prod r Text Lexeme Adjective
                                       -> Prod r Text Lexeme Container
                                       -> Grammar r (Prod r Text Lexeme ContainerPhrase)
containerPhraseRules determinerRule adjRule containerRule = do
  containmentMarker <- parseRule containmentMarkers ContainmentMarker
  nounPhraseRule rules >>= \nounPhrase ->
    rule $ ContainerPhrase <$> containmentMarker <*> nounPhrase
  where
   rules
      = NounPhraseRules
          { _determinerRule = determinerRule
          , _adjRule = adjRule
          , _nounRule = containerRule
          }
--  DirectionalStimulusNounPhrase DirectionalStimulusMarker (NounPhrase DirectionalStimulus)

directionalStimulusNounPhraseRules :: Prod r Text Lexeme Determiner
                                       -> Prod r Text Lexeme Adjective
                                       -> Prod r Text Lexeme DirectionalStimulusMarker
                                       -> Prod r Text Lexeme DirectionalStimulus
                                       -> Grammar r (Prod r Text Lexeme DirectionalStimulusNounPhrase)
directionalStimulusNounPhraseRules determinerRule adjRule directionalStimulusMarker directionalStimulusRule =
  nounPhraseRule rules >>= \nounPhrase ->
    rule $ DirectionalStimulusNounPhrase <$> directionalStimulusMarker <*> nounPhrase
  where
   rules
      = NounPhraseRules
          { _determinerRule = determinerRule
          , _adjRule = adjRule
          , _nounRule = directionalStimulusRule
          }

consumableNounPhraseRules :: Prod r Text Lexeme Determiner
                                       -> Prod r Text Lexeme Adjective
                                       -> Prod r Text Lexeme Consumable
                                       -> Grammar r (Prod r Text Lexeme ConsumableNounPhrase)
consumableNounPhraseRules determinerRule adjRule directionalStimulusRule =
  nounPhraseRule rules >>= \nounPhrase ->
    rule $ ConsumableNounPhrase <$> nounPhrase
  where
   rules
      = NounPhraseRules
          { _determinerRule = determinerRule
          , _adjRule = adjRule
          , _nounRule = directionalStimulusRule
          }

nounPhraseRule :: NounPhraseRules a r
                    -> Grammar r (Prod r Text Lexeme (NounPhrase a))
nounPhraseRule (NounPhraseRules{..}) =
  rule $ SimpleNounPhrase <$> _nounRule
           <|> NounPhrase <$> _determinerRule <*> _nounRule
           <|> DescriptiveNounPhrase <$> _adjRule <*> _nounRule
           <|> DescriptiveNounPhraseDet
                 <$> _determinerRule
                 <*> _adjRule
                 <*> _nounRule

objectPhraseRules :: Prod r Text Lexeme Determiner
                                       -> Prod r Text Lexeme Adjective
                                       -> Prod r Text Lexeme Objective
                                       -> Grammar r (Prod r Text Lexeme ObjectPhrase)
objectPhraseRules determinerRule adjRule objectiveRule =
  nounPhraseRule rules >>= \nounPhrase ->
    rule $ ObjectPhrase <$> nounPhrase
  where
   rules
      = NounPhraseRules
          { _determinerRule = determinerRule
          , _adjRule = adjRule
          , _nounRule = objectiveRule
          }

somaticStimulusNounPhraseRules :: Prod r Text Lexeme Determiner
                                       -> Prod r Text Lexeme Adjective
                                       -> Prod r Text Lexeme SomaticStimulus
                                       -> Grammar r (Prod r Text Lexeme SomaticStimulusNounPhrase)
somaticStimulusNounPhraseRules determinerRule adjRule somaticStimulusRule =
  nounPhraseRule rules >>= \nounPhrase ->
    rule $ SomaticStimulusNounPhrase <$> nounPhrase
  where
   rules
      = NounPhraseRules
          { _determinerRule = determinerRule
          , _adjRule = adjRule
          , _nounRule = somaticStimulusRule
          }

surfacePhraseRules :: Prod r Text Lexeme Determiner
                        -> Prod r Text Lexeme Adjective
                        -> Prod r Text Lexeme Surface
                        -> Grammar r (Prod r Text Lexeme SurfacePhrase)
surfacePhraseRules determinerRule adjRule surfaceRule = do
  surfaceMarker <- parseRule surfaceMarkers SurfaceMarker
  nounPhraseRule rules >>= \nounPhrase ->
    rule $ SimpleSurfacePhrase <$> nounPhrase
             <|> SurfacePhrase <$> surfaceMarker <*> nounPhrase
  where
   rules
      = NounPhraseRules
          { _determinerRule = determinerRule
          , _adjRule = adjRule
          , _nounRule = surfaceRule
          }

supportPhraseRules :: Prod r Text Lexeme Determiner
                        -> Prod r Text Lexeme Adjective
                        -> Grammar r (Prod r Text Lexeme SupportPhrase)
supportPhraseRules determinerRule adjRule = do
  surface <- parseRule surfaces Surface
  surfacePhrase <- surfacePhraseRules determinerRule adjRule surface
  container <- parseRule containers Container
  containerPhrase <- containerPhraseRules determinerRule adjRule container
  pure $ SurfaceSupport <$> surfacePhrase
   <|> ContainerSupport <$> containerPhrase
