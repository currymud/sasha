module Parser.Rules.Composites.Nouns where

import           Control.Applicative               (Alternative ((<|>)))
import           Data.Kind                         (Type)
import           Data.Text                         (Text, unwords)
import           GHC.Generics                      (Generic)
import           Lexer                             (Lexeme (..))
import           Model.Parser.Atomics.Adjectives   (Adjective)
import           Model.Parser.Atomics.Adverbs      (ImplicitPath,
                                                    ModToggleAdverb)
import           Model.Parser.Atomics.Misc         (Determiner)
import           Model.Parser.Atomics.Nouns        (Container,
                                                    DirectionalStimulus,
                                                    ModToggleNoun, ObjectPath,
                                                    Objective, SimpleAccessNoun,
                                                    Surface, TargetedStimulus,
                                                    ToggleNoun)
import           Model.Parser.Atomics.Prepositions (ContainmentMarker,
                                                    InstrumentalMarker, Path,
                                                    SurfaceMarker,
                                                    TargetedStimulusMarker)
import           Model.Parser.Composites.Nouns     (ContainerPhrase,
                                                    ContainerPhraseRules (..),
                                                    DirectionalStimulusNounPhrase,
                                                    DirectionalStimulusNounRules (..),
                                                    ModToggleNounPhrase,
                                                    ModToggleNounPhraseRules (..),
                                                    NounPhrase,
                                                    NounPhraseRules (..),
                                                    ObjectPathPhrase,
                                                    ObjectPathPhraseRules (..),
                                                    ObjectPhrase,
                                                    ObjectPhraseRules (..),
                                                    PathPhrase,
                                                    PathPhraseRules (..),
                                                    PrepObjectPhrase,
                                                    PrepObjectPhraseRules (..),
                                                    SimpleAccessNounPhrase,
                                                    SimpleAccessNounPhraseRules (..),
                                                    SupportPhrase,
                                                    SupportPhraseRules (..),
                                                    SurfacePhrase,
                                                    SurfacePhraseRules (..),
                                                    TargetedStimulusNounPhrase,
                                                    TargetedStimulusNounPhraseRules (..),
                                                    ToggleNounPhrase,
                                                    ToggleNounPhraseRules (..))
import           Prelude                           hiding (unwords)
import           Relude.String.Conversion          (ToText, toText)
import           Text.Earley                       (Grammar)
import           Text.Earley.Grammar               (Prod, rule)

-- (runStateT . runExceptT) (runReaderT start config) defaultGameState
-- Plant the pot plant in the plant pot with the trowel
-- unlock the cabinet below the shelf

objectPathPhraseRules :: ObjectPathPhraseRules r
                          -> Grammar r (Prod r Text Lexeme ObjectPathPhrase)
objectPathPhraseRules (ObjectPathPhraseRules {..}) =
  rule $ SimpleObjectPathPhrase <$> _objectPathRule
           <|> ObjectPathPhrase <$> _determinerRule <*> _objectPathRule
           <|> ObjectPathPhraseAdj
                 <$> _determinerRule
                 <*> _adjRule
                 <*> _objectPathRule

pathPhraseRule :: PathPhraseRules r
                  -> Grammar r (Prod r Text Lexeme PathPhrase)
pathPhraseRule (PathPhraseRules {..}) =
  rule $ SimplePath <$> _implicitPathRule
           <|> PathPhrase <$> _pathRule <*> _determinerRule <*> _objectPathRule

prepObjectPhraseRule :: PrepObjectPhraseRules r
                        -> Grammar r (Prod r Text Lexeme PrepObjectPhrase)
prepObjectPhraseRule (PrepObjectPhraseRules {..}) =
  rule $ Instrument <$> _instrumentalMarkerRule <*> _objectPathPhraseRule

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

objectPhraseRule :: ObjectPhraseRules r
                      -> Grammar r (Prod r Text Lexeme ObjectPhrase)
objectPhraseRule (ObjectPhraseRules {..}) =
  nounPhraseRule rules >>= \nounPhrase ->
    rule $ ObjectPhrase <$> nounPhrase
  where
   rules
      = NounPhraseRules
          { _determinerRule = _determinerRule
          , _adjRule = _adjRule
          , _nounRule = _objectiveRule
          }

surfacePhraseRule :: SurfacePhraseRules r
                      -> Grammar r (Prod r Text Lexeme SurfacePhrase)
surfacePhraseRule (SurfacePhraseRules{..}) =
  nounPhraseRule rules >>= \nounPhrase ->
    rule $ SimpleSurfacePhrase <$> nounPhrase
             <|> SurfacePhrase <$> _surfaceMarkerRule <*> nounPhrase
  where
   rules
      = NounPhraseRules
          { _determinerRule = _determinerRule
          , _adjRule = _adjRule
          , _nounRule = _surfaceRule
          }

containerPhraseRule :: ContainerPhraseRules r
                        -> Grammar r (Prod r Text Lexeme ContainerPhrase)
containerPhraseRule (ContainerPhraseRules{..}) =
  nounPhraseRule rules >>= \nounPhrase ->
    rule $ SimpleContainerPhrase <$> nounPhrase
             <|> ContainerPhrase <$> _containerMarkerRule <*> nounPhrase
  where
   rules
      = NounPhraseRules
          { _determinerRule = _determinerRule
          , _adjRule = _adjRule
          , _nounRule = _containerRule
          }

supportPhraseRule :: SupportPhraseRules r
                     -> Grammar r (Prod r Text Lexeme SupportPhrase)
supportPhraseRule (SupportPhraseRules{..}) =
  rule $ SurfaceSupport <$> _surfacePhraseRule
           <|> ContainerSupport <$> _containerPhraseRule

directionalStimulusNounPhraseRule :: DirectionalStimulusNounRules r
                                -> Grammar r (Prod r Text Lexeme DirectionalStimulusNounPhrase)
directionalStimulusNounPhraseRule (DirectionalStimulusNounRules{..}) =
  nounPhraseRule rules >>= \nounPhrase ->
    rule $ DirectionalStimulusNounPhrase <$> nounPhrase
  where
   rules
      = NounPhraseRules
          { _determinerRule = _determinerRule
          , _adjRule = _adjRule
          , _nounRule = _directionalStimulusRule
          }

toggleNounPhraseRule :: ToggleNounPhraseRules r
                        -> Grammar r (Prod r Text Lexeme ToggleNounPhrase)
toggleNounPhraseRule (ToggleNounPhraseRules{..}) =
  nounPhraseRule rules >>= \nounPhrase ->
    rule $ ToggleNounPhrase <$> nounPhrase
  where
   rules
      = NounPhraseRules
          { _determinerRule = _determinerRule
          , _adjRule = _adjRule
          , _nounRule = _toggleRule
          }

modToggleNounPhraseRule :: ModToggleNounPhraseRules r
                        -> Grammar r (Prod r Text Lexeme ModToggleNounPhrase)
modToggleNounPhraseRule (ModToggleNounPhraseRules{..}) =
  nounPhraseRule rules >>= \nounPhrase ->
    rule $ ModToggleNounPhrase <$> nounPhrase <*> _modToggleAdvRule
  where
   rules
      = NounPhraseRules
          { _determinerRule = _determinerRule
          , _adjRule = _adjRule
          , _nounRule = _modToggleNounRule
          }

targetedStimulusNounPhraseRule :: TargetedStimulusNounPhraseRules r
                                  -> Grammar r (Prod r Text Lexeme TargetedStimulusNounPhrase)
targetedStimulusNounPhraseRule (TargetedStimulusNounPhraseRules{..}) =
  nounPhraseRule rules >>= \nounPhrase ->
    rule $ SimpleAgentStimulus <$> _targetedStimulusRule
             <|> TargetedStimuliiNounPhrase <$> _targetedStimulusMarkerRule <*> nounPhrase
  where
   rules
      = NounPhraseRules
          { _determinerRule = _determinerRule
          , _adjRule = _adjRule
          , _nounRule = _targetedStimulusRule
          }

simpleAccessNounPhraseRule :: SimpleAccessNounPhraseRules r
                          -> Grammar r
                               (Prod r Text Lexeme SimpleAccessNounPhrase)
simpleAccessNounPhraseRule (SimpleAccessNounPhraseRules{..}) =
  nounPhraseRule rules >>= \nounPhrase ->
    rule $ SimpleAccessNounPhrase <$> nounPhrase
  where
   rules
      = NounPhraseRules
          { _determinerRule = _determinerRule
          , _adjRule = _adjRule
          , _nounRule = _simpleAccessNounRule
          }

