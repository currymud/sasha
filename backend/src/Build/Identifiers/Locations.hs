{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Build.Identifiers.Locations (bedroomInBed'
                                   , bedroomInBedGID
                                   , locationMap ) where

import           Build.BedPuzzle.BedRoom        (bedroomInBed)
import           Build.Templates.Identification (makeLabels,
                                                 makeLocationGIDsAndMap)
import           Grammar.Parser.Lexer           (Lexeme (BED))
import           Model.Label                    (Label (Label))

makeLabels [ ([| bedroomInBed |],BED)]

makeLocationGIDsAndMap [ [| bedroomInBed |] ]
