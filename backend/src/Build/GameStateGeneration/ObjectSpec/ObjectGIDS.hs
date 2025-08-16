module Build.GameStateGeneration.ObjectSpec.ObjectGIDS where
import           Build.Templates.Identification                      (declareObjectGIDs)
import           Grammar.Parser.Partitions.Adjectives                (small)
import           Grammar.Parser.Partitions.Misc                      (the)
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus (chair,
                                                                      floor,
                                                                      mail,
                                                                      pill,
                                                                      pocket,
                                                                      robe,
                                                                      table)
import           Model.Parser.Atomics.Nouns                          (DirectionalStimulus)
import           Model.Parser.Composites.Nouns
import           Prelude                                             hiding
                                                                     (floor)

declareObjectGIDs [ (SimpleNounPhrase chair, Nothing)                      -- chairGID
                  , (DescriptiveNounPhraseDet the small table, Nothing)    -- smallTableGID
                  , (SimpleNounPhrase pill, Nothing)                       -- pillGID
                  , (SimpleNounPhrase mail, Nothing)                       -- mailGID
                  , (SimpleNounPhrase robe, Nothing)                       -- robeGID
                  , (SimpleNounPhrase pocket, Nothing)                     -- pocketGID
                  , (SimpleNounPhrase floor, Nothing)                      -- floorGID
                  ]


