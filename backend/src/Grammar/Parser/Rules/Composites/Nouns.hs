module Grammar.Parser.Rules.Composites.Nouns where
import           Control.Applicative             (Alternative ((<|>)))
import           Data.Text                       (Text)
import           Model.Parser.Atomics.Adjectives (Adjective)
import           Model.Parser.Atomics.Misc       (Determiner)
import           Model.Parser.Atomics.Nouns      (DirectionalStimulus, Edible)
import           Model.Parser.Composites.Nouns   (DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                  EdibleNounPhrase (EdibleNounPhrase),
                                                  NounPhrase (DescriptiveNounPhrase, DescriptiveNounPhraseDet, NounPhrase, SimpleNounPhrase),
                                                  NounPhraseRules (NounPhraseRules, _adjRule, _determinerRule, _nounRule))
import           Model.Parser.Lexer              (Lexeme)
import           Text.Earley                     (Grammar)
import           Text.Earley.Grammar             (Prod, rule)

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
