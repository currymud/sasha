module Grammar.Parser.Rules.Composites.Nouns where
import           Control.Applicative                      (Alternative ((<|>)))
import           Data.Text                                (Text)
import           Grammar.Parser.Partitions.Nouns.Surfaces (surfaces)
import           Grammar.Parser.Partitions.Prepositions   (surfaceMarkers)
import           Grammar.Parser.Rules.Atomics.Utils       (parseRule)
import           Model.Parser.Atomics.Adjectives          (Adjective)
import           Model.Parser.Atomics.Misc                (Determiner)
import           Model.Parser.Atomics.Nouns               (Container,
                                                           DirectionalStimulus,
                                                           Edible,
                                                           SomaticStimulus,
                                                           Surface (Surface))
import           Model.Parser.Atomics.Prepositions        (SurfaceMarker (SurfaceMarker))
import           Model.Parser.Composites.Nouns            (ContainerPhrase,
                                                           DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                           EdibleNounPhrase (EdibleNounPhrase),
                                                           NounPhrase (DescriptiveNounPhrase, DescriptiveNounPhraseDet, NounPhrase, SimpleNounPhrase),
                                                           NounPhraseRules (NounPhraseRules, _adjRule, _determinerRule, _nounRule),
                                                           SomaticStimulusNounPhrase (SomaticStimulusNounPhrase),
                                                           SurfacePhrase (SimpleSurfacePhrase, SurfacePhrase))
import           Model.Parser.Lexer                       (Lexeme)
import           Text.Earley                              (Grammar)
import           Text.Earley.Grammar                      (Prod, rule)

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

containerPhraseRules :: Prod r Text Lexeme Determiner
                                       -> Prod r Text Lexeme Adjective
                                       -> Prod r Text Lexeme Container
                                       -> Grammar r (Prod r Text Lexeme ContainerPhrase)
containerPhraseRules determinerRule adjRule containerRule =
  nounPhraseRule rules >>= \nounPhrase ->
    rule $ SimpleContainerPhrase <$> nounPhrase
             <|> ContainerPhrase <$> containmentMarker <*> nounPhrase
  where
   rules
      = NounPhraseRules
          { _determinerRule = determinerRule
          , _adjRule = adjRule
          , _nounRule = containerRule
          }
directionalStimulusNounPhraseRules :: Prod r Text Lexeme Determiner
                                       -> Prod r Text Lexeme Adjective
                                       -> Prod r Text Lexeme DirectionalStimulus
                                       -> Grammar r (Prod r Text Lexeme DirectionalStimulusNounPhrase)
directionalStimulusNounPhraseRules determinerRule adjRule directionalStimulusRule =
  nounPhraseRule rules >>= \nounPhrase ->
    rule $ DirectionalStimulusNounPhrase <$> nounPhrase
  where
   rules
      = NounPhraseRules
          { _determinerRule = determinerRule
          , _adjRule = adjRule
          , _nounRule = directionalStimulusRule
          }

edibleNounPhraseRules :: Prod r Text Lexeme Determiner
                                       -> Prod r Text Lexeme Adjective
                                       -> Prod r Text Lexeme Edible
                                       -> Grammar r (Prod r Text Lexeme EdibleNounPhrase)
edibleNounPhraseRules determinerRule adjRule directionalStimulusRule =
  nounPhraseRule rules >>= \nounPhrase ->
    rule $ EdibleNounPhrase <$> nounPhrase
  where
   rules
      = NounPhraseRules
          { _determinerRule = determinerRule
          , _adjRule = adjRule
          , _nounRule = directionalStimulusRule
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

