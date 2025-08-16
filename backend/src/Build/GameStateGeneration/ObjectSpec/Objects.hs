module Build.GameStateGeneration.ObjectSpec.Objects where


import           Build.GameStateGeneration.ObjectSpec                    (defaultObject,
                                                                          withBehaviors,
                                                                          withDescription,
                                                                          withDescriptives,
                                                                          withShortName)
import           Build.GameStateGeneration.ObjectSpec.ObjectGIDS         (chairGID)
import           Build.Identifiers.Actions                               (seeChairFGID,
                                                                          seeTableGID)
import           Build.Templates.Identification                          (makeObjectMap)
import           Data.Function                                           ((&))
import           Grammar.Parser.Partitions.Adjectives                    (small)
import           Grammar.Parser.Partitions.Misc                          (the)
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus     (chair,
                                                                          table)
import qualified Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import           Model.GameState                                         (ActionManagement (DSAManagementKey),
                                                                          Object)
import           Model.Parser.Composites.Nouns                           (NounPhrase (DescriptiveNounPhraseDet, SimpleNounPhrase))

chairObj :: Object
chairObj = defaultObject
  & withShortName "a chair"
  & withDescription "It's the chair next to your bed"
  & withDescriptives [SimpleNounPhrase chair, DescriptiveNounPhraseDet the small chair]
  & withBehaviors [DSAManagementKey Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb.look seeChairFGID]


smallTableObj :: Object
smallTableObj = defaultObject
  & withShortName "small table"
  & withDescription "A small bedside table"
  & withDescriptives [DescriptiveNounPhraseDet the small table]
  & withBehaviors [DSAManagementKey Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb.look seeTableGID]

makeObjectMap [ ('chairGID, 'chairObj) ]  -- objectMap :: GIDToDataMap Object Object
