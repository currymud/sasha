{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
module Build.Identifiers where
import           Actions.Percieve.Look          (agentCanSee)
import           Build.BedPuzzle.Actions.Look   (pitchBlackF)
import           Build.Templates.Identification (makeActionGIDsAndMap,
                                                 makeLabels)

import           Grammar.Parser.Lexer           (Lexeme (BED))
import           Model.GameState                (Location)
import           Model.Label                    (Label (Label))

makeActionGIDsAndMap [[| agentCanSee |], [| pitchBlackF |]]

makeLabels ''Location [("bedRoomInBed", BED)]

