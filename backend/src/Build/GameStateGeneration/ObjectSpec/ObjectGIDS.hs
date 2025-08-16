module Build.GameStateGeneration.ObjectSpec.ObjectGIDS where
import           Build.Templates.Identification                      (declareObjectGIDs)
import           Grammar.Parser.Partitions.Adjectives                (small)
import           Grammar.Parser.Partitions.Misc                      (the)
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus (chair,
                                                                      pill,
                                                                      table)
import           Model.Parser.Atomics.Nouns                          (DirectionalStimulus)
import           Model.Parser.Composites.Nouns


declareObjectGIDs [ (SimpleNounPhrase chair, Nothing)                      -- chairGID
                  , (DescriptiveNounPhraseDet the small table, Nothing)    -- smallTableGID
                  , (DescriptiveNounPhraseDet the small table, Just "bedroom")  -- smallTableBedroomGID
                  , (DescriptiveNounPhraseDet the small table, Just "kitchen")  -- smallTableKitchenGID
                  ]


