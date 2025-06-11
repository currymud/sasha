{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Parser.Rule where
import           Control.Applicative                     (Alternative ((<|>)))
import           Data.HashSet                            (HashSet, toList)
import           Data.Kind                               (Type)
import           Data.List                               (find)
import           Data.Text                               (Text)
import           Debug.Trace                             (trace, traceShow)
import           Lexer                                   (HasLexeme,
                                                          Lexeme (..), toLexeme)
import           Parser.CGModel                          (AcquisitionVerbPhrase (..),
                                                          AdjPhrase (..),
                                                          ContainerPhrase (..),
                                                          GeneralPlacementVerbPhrase (..),
                                                          Imperative (..),
                                                          Interrogative (..),
                                                          NounPhrase (..),
                                                          ObjectPhrase (..),
                                                          TransferVerbPhrase (..),
                                                          Vocative (..))
import           Parser.SpeechParts.Atomics.Adjectives   (Adjective)
import           Parser.SpeechParts.Atomics.Adverbs      (ImplicitPath)
import           Parser.SpeechParts.Atomics.Misc         (Determiner, Partition)
import           Parser.SpeechParts.Atomics.Nouns        (Agent, Container,
                                                          Objective)
import           Parser.SpeechParts.Atomics.Prepositions (LocationInterrogativeMarker,
                                                          ObjectInterrogativeMarker,
                                                          SourceMarker,
                                                          TopicMarker)
import           Parser.SpeechParts.Atomics.Verbs        (AcquisitionVerb,
                                                          CardinalMovementVerb,
                                                          Copula,
                                                          GeneralPlacementVerb,
                                                          ImplicitRegionalStimulus,
                                                          StimulusVerb)
import           Text.Earley                             (Prod)
import           Text.Earley.Grammar                     (Grammar, rule,
                                                          terminal)

parseRule :: (HasLexeme a, Show a) => HashSet a -> (Lexeme -> a) -> Grammar r (Prod r Text Lexeme a)
parseRule morphemes toMorpheme = traceShow ("trying parseRule" <> show morphemes) rule $ terminal $ maybeMorpheme morphemes toMorpheme


type ImperativeRules :: (Type -> Type -> Type -> Type) -> Type
data ImperativeRules r = ImperativeRules
  { _generalPlacementVerbRule :: Prod r Text Lexeme GeneralPlacementVerbPhrase
  , _acquisitionVerbRule      :: Prod r Text Lexeme AcquisitionVerbPhrase
  , _stimulusVerbRule         :: Prod r Text Lexeme StimulusVerb
  , _cardinalMovementVerbRule :: Prod r Text Lexeme CardinalMovementVerb
  , _implicitPathRule         :: Prod r Text Lexeme ImplicitPath
  , _implicitRegionalStimulusRule :: Prod r Text Lexeme ImplicitRegionalStimulus
  }

imperativeRule :: ImperativeRules r
                   -> Grammar r (Prod r Text Lexeme Imperative)
imperativeRule (ImperativeRules{..}) =
  rule $ GeneralPlacement <$> _generalPlacementVerbRule
           <|> AcquisitionalPlacement <$> _acquisitionVerbRule
           <|> ImplicitStimulusVerb <$> _stimulusVerbRule
           <|> CardinalMovement <$> _cardinalMovementVerbRule <*> _implicitPathRule
           <|> ImplicitRegionalStimulusVerb <$> _implicitRegionalStimulusRule

objectPhraseRule :: Prod r Text Lexeme Determiner
                      -> Prod r Text Lexeme AdjPhrase
                      -> Prod r Text Lexeme Objective
                      -> Grammar r (Prod r Text Lexeme ObjectPhrase)
objectPhraseRule determinerRule adjPhrase objectiveRule =
  rule $ ObjectPhrase <$> nounPhrase
  where
    nounPhrase
      = SimpleNounPhrase <$> objectiveRule
          <|> NounPhrase <$> determinerRule <*> objectiveRule
          <|> DescriptiveNounPhrase <$> adjPhrase <*> objectiveRule

type ContainerPhraseRules :: (Type -> Type -> Type -> Type) -> Type
data ContainerPhraseRules r = ContainerPhraseRules
  { _determinerPhraseRule :: Prod r Text Lexeme Determiner
  , _adjPhraseRule        :: Prod r Text Lexeme AdjPhrase
  , _containerRule        :: Prod r Text Lexeme Container
  }

containerPhraseRule :: ContainerPhraseRules r
                        -> Grammar r (Prod r Text Lexeme ContainerPhrase)
containerPhraseRule (ContainerPhraseRules{..}) =
 rule $ ContainerPhrase <$> nounPhrase
 where
   nounPhrase = SimpleNounPhrase <$> _containerRule
          <|> NounPhrase <$> _determinerPhraseRule <*> _containerRule
          <|> DescriptiveNounPhrase <$> _adjPhraseRule <*> _containerRule
            {-
adjPhraseRule :: Prod r Text Lexeme Adjective
                  -> Prod r Text Lexeme Determiner
                  -> Prod r Text Lexeme AdjPhrase
                  -> Grammar r (Prod r Text Lexeme AdjPhrase)
adjPhraseRule adj determiner adjphrase =
  rule $ SimpleAdjPhrase <$> adj
           <|> SimpleAdjPhraseDet <$> determiner <*> adj
           <|> AdjPhrase <$> adj <*> adjphrase
           <|> AdjPhraseDet <$> determiner <*> adj <*> adjphrase
-}
type GeneralPlacementVerbPhraseRules :: (Type -> Type -> Type -> Type) -> Type
data GeneralPlacementVerbPhraseRules r = GeneralPlacementVerbPhraseRules
  { _generalPlacementVerbRule :: Prod r Text Lexeme GeneralPlacementVerb
  , _objectPhraseRule         :: Prod r Text Lexeme ObjectPhrase
  , _sourceMarkerRule         :: Prod r Text Lexeme SourceMarker
  , _containerPhraseRule      :: Prod r Text Lexeme ContainerPhrase
  }

generalPlacementVerbPhrase :: GeneralPlacementVerbPhraseRules r
                              -> Grammar r (Prod r Text Lexeme GeneralPlacementVerbPhrase)
generalPlacementVerbPhrase (GeneralPlacementVerbPhraseRules{..}) =
  rule $ GeneralPlacementVerbPhrase <$> transferVerbPhrase
  where
    transferVerbPhrase = simpleTransferVerbPhrase <|> generalTransferVerbPhrase

    simpleTransferVerbPhrase = SimpleTransferVerbPhrase
                                  <$> _generalPlacementVerbRule
                                  <*> _objectPhraseRule
    generalTransferVerbPhrase = TransferVerbPhrase
                                    <$> _generalPlacementVerbRule
                                    <*> _objectPhraseRule
                                    <*> _sourceMarkerRule
                                    <*> _containerPhraseRule

type AcquisitionVerbPhraseRules :: (Type -> Type -> Type -> Type) -> Type
data AcquisitionVerbPhraseRules r = AcquisitionVerbPhraseRules
  { _acquisitionVerbRule :: Prod r Text Lexeme AcquisitionVerb
  , _objectPhraseRule    :: Prod r Text Lexeme ObjectPhrase
  , _sourceMarkerRule    :: Prod r Text Lexeme SourceMarker
  , _containerPhraseRule :: Prod r Text Lexeme ContainerPhrase
  }

acquisitionVerbPhrase :: AcquisitionVerbPhraseRules r
                              -> Grammar r (Prod r Text Lexeme AcquisitionVerbPhrase)
acquisitionVerbPhrase (AcquisitionVerbPhraseRules{..}) =
  rule $ AcquisitionVerbPhrase <$> transferVerbPhrase
  where
    transferVerbPhrase = simpleTransferVerbPhrase <|> generalTransferVerbPhrase
    simpleTransferVerbPhrase = SimpleTransferVerbPhrase
                                    <$> _acquisitionVerbRule
                                    <*> _objectPhraseRule
    generalTransferVerbPhrase = TransferVerbPhrase
                                <$> _acquisitionVerbRule
                                <*> _objectPhraseRule
                                <*> _sourceMarkerRule
                                <*> _containerPhraseRule


type InterrogativeRules :: (Type -> Type -> Type -> Type) -> Type
data InterrogativeRules r = InterrogativeRules
  { _objectInterrogativeMarkerRule :: Prod r Text Lexeme ObjectInterrogativeMarker
  , _copulaRule                    :: Prod r Text Lexeme Copula
  , _topicMarkerRule               :: Prod r Text Lexeme TopicMarker
  , _LocationInterrogativeMarkerRule :: Prod r Text Lexeme LocationInterrogativeMarker
  , _objectPhraseRule              :: Prod r Text Lexeme ObjectPhrase
  }

interrogativeRule :: InterrogativeRules r
                     -> Grammar r (Prod r Text Lexeme Interrogative)
interrogativeRule (InterrogativeRules{..}) =
 rule $ trace "trying objectInterrogative" objectInterrogative
          <|> trace "trying location interrogative" locationInterrogative
 where
   objectInterrogative = ObjectInterrogative
                           <$> _objectInterrogativeMarkerRule
                           <*> _topicMarkerRule
                           <*> _objectPhraseRule

   locationInterrogative = LocationInterrogative
                             <$> _LocationInterrogativeMarkerRule
                             <*> _copulaRule
                             <*> _objectPhraseRule


type VocativeRules :: (Type -> Type -> Type -> Type) -> Type
data VocativeRules r = VocativeRules
  { _agentRule         :: Prod r Text Lexeme Agent
  , _partitionRule     :: Prod r Text Lexeme Partition
  , _imperativeRule    :: Prod r Text Lexeme Imperative
  , _interrogativeRule :: Prod r Text Lexeme Interrogative
  }

vocativeRule :: VocativeRules r
                -> Grammar r (Prod r Text Lexeme Vocative)
vocativeRule (VocativeRules{..}) =
  rule $ vocativeImperative  <|> vocativeInterrogative
  where
    vocativeImperative = VocativeImperative
                           <$> _agentRule
                           <*>  _partitionRule
                           <*> _imperativeRule

    vocativeInterrogative = VocativeInterrogative
                              <$> _agentRule
                              <*> _partitionRule
                              <*> _interrogativeRule

maybeMorpheme :: (HasLexeme a) => HashSet a -> (Lexeme -> a) -> Lexeme -> Maybe a
maybeMorpheme morphemes toMorpheme lexeme =
  case find (\m -> toLexeme m == lexeme) (toList morphemes) of
    Just _  -> Just (toMorpheme lexeme)
    Nothing -> Nothing

