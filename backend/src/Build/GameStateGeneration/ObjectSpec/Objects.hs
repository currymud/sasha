module Build.GameStateGeneration.ObjectSpec.Objects where


import           Build.GameStateGeneration.ObjectSpec                    (defaultObject,
                                                                          withBehaviors,
                                                                          withDescription,
                                                                          withDescriptives,
                                                                          withShortName)
import           Build.GameStateGeneration.ObjectSpec.ObjectGIDS         (chairGID,
                                                                          smallTableGID)
import           Build.Identifiers.Actions                               (alreadyHaveRobeFGID,
                                                                          seeChairFGID,
                                                                          seeMailGID,
                                                                          seeRobeChairGID,
                                                                          seeTableGID,
                                                                          takePillDeniedFGID,
                                                                          whatPillGID)
import           Build.Templates.Identification                          (makeObjectMap)
import           Data.Function                                           ((&))
import           Grammar.Parser.Partitions.Adjectives                    (small)
import           Grammar.Parser.Partitions.Misc                          (the)
import qualified Grammar.Parser.Partitions.Nouns.DirectionalStimulus     (chair,
                                                                          floor,
                                                                          mail,
                                                                          pill,
                                                                          pocket,
                                                                          robe,
                                                                          table)

import           Build.BedPuzzle.Actions.Objects.Pill                    (takePillCVP)
import           Build.BedPuzzle.Actions.Objects.Robe                    (getRobeAVP)
import qualified Grammar.Parser.Partitions.Nouns.Consumables             (pill)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs        (get)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import           Model.GameState                                         (ActionManagement (AAManagementKey, CAManagementKey, DSAManagementKey),
                                                                          Object)
import           Model.Parser.Atomics.Nouns                              (DirectionalStimulus)
import           Model.Parser.Composites.Nouns                           (NounPhrase (DescriptiveNounPhraseDet, SimpleNounPhrase))


chairDS :: DirectionalStimulus
chairDS = Grammar.Parser.Partitions.Nouns.DirectionalStimulus.chair

chairObj :: Object
chairObj = defaultObject
  & withShortName "a chair"
  & withDescription "It's the chair next to your bed"
  & withDescriptives [SimpleNounPhrase chairDS, DescriptiveNounPhraseDet the small chairDS]
  & withBehaviors [DSAManagementKey Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb.look seeChairFGID]


tableDS :: DirectionalStimulus
tableDS = Grammar.Parser.Partitions.Nouns.DirectionalStimulus.table

smallTableObj :: Object
smallTableObj = defaultObject
  & withShortName "small table"
  & withDescription "A small bedside table"
  & withDescriptives [DescriptiveNounPhraseDet the small tableDS]
  & withBehaviors [DSAManagementKey Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb.look seeTableGID]

pillDS :: DirectionalStimulus
pillDS = Grammar.Parser.Partitions.Nouns.DirectionalStimulus.pill

pillObj :: Object
pillObj = defaultObject
  & withShortName "pill"
  & withDescription "A small, round pill. Probably good for headaches."
  & withDescriptives [SimpleNounPhrase pillDS]
  & withBehaviors [DSAManagementKey look whatPillGID, CAManagementKey takePillCVP takePillDeniedFGID]

mailDS :: DirectionalStimulus
mailDS = Grammar.Parser.Partitions.Nouns.DirectionalStimulus.mail

mailObj :: Object
mailObj = defaultObject
  & withShortName "mail"
  & withDescription "Some mail on the table"
  & withDescriptives [SimpleNounPhrase mailDS]
  & withBehaviors [DSAManagementKey look seeMailGID]


robeDS :: DirectionalStimulus
robeDS = Grammar.Parser.Partitions.Nouns.DirectionalStimulus.robe

robeObj :: Object
robeObj = defaultObject
  & withShortName "robe"
  & withDescription "A comfortable robe"
  & withDescriptives [SimpleNounPhrase robeDS]
  & withBehaviors [DSAManagementKey look seeRobeChairGID,
                   AAManagementKey getRobeAVP alreadyHaveRobeFGID]  -- Get behavior

pocketDS :: DirectionalStimulus
pocketDS = Grammar.Parser.Partitions.Nouns.DirectionalStimulus.pocket

pocketObj :: Object
pocketObj = defaultObject
  & withShortName "pocket"
  & withDescription "A pocket in the robe"
  & withDescriptives [SimpleNounPhrase pocketDS]

floorDS :: DirectionalStimulus
floorDS = Grammar.Parser.Partitions.Nouns.DirectionalStimulus.floor

floorObj :: Object
floorObj = defaultObject
  & withShortName "floor"
  & withDescription "The bedroom floor"
  & withDescriptives [SimpleNounPhrase floorDS]

makeObjectMap [ ('chairGID, 'chairObj), ('smallTableGID, 'smallTableObj) ]  -- objectMap :: GIDToDataMap Object Object
