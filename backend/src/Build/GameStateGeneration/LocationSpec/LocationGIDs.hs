module Build.GameStateGeneration.LocationSpec.LocationGIDs where

import           Build.Templates.Identification                      (declareLocationGIDs,
                                                                      nounPhrase,
                                                                      nounPhraseWithSuffix)
import           Grammar.Parser.Partitions.Adjectives                (small)
import           Grammar.Parser.Partitions.Misc                      (the)
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus (bedroom)
import           Model.Parser.Composites.Nouns                       (NounPhrase (DescriptiveNounPhraseDet, SimpleNounPhrase))

-- Generate location GIDs using noun phrases
declareLocationGIDs [ nounPhrase (SimpleNounPhrase bedroom)                    -- bedroomGID
                    , nounPhraseWithSuffix (SimpleNounPhrase bedroom) "upstairs"  -- bedroomUpstairsGID
                    ]
