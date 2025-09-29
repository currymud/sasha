{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module ActionDiscovery.Administrative (manageAdministration) where

import           Control.Monad.Identity     (Identity)
import qualified Data.Map.Strict
import           GameState                  (getPlayerM, modifyNarration,
                                             updateActionConsequence)
import           Model.Core                 (GameComputation,
                                             GameState (_effectRegistry, _world),
                                             World (..))

import           Control.Monad.State        (gets)
import           Grammar.Parser.Lexer       (Lexeme (DEBUG))
import           Model.Core.Mappings        (GIDToDataMap (GIDToDataMap))
import           Model.Parser.Atomics.Verbs (AdministrativeVerb (AdministrativeVerb))

manageAdministration :: AdministrativeVerb -> GameComputation Identity ()
manageAdministration (AdministrativeVerb  DEBUG) = do
     modifyNarration $ updateActionConsequence "=== GAME STATE DUMP ==="

      -- Dump effect registry
     effectRegistry <- gets _effectRegistry

     pure ()

      -- Dump player info
     player <- getPlayerM
     pure ()

      -- Dump world info
     world <- gets _world
     let GIDToDataMap objectMap = _objectMap world
         GIDToDataMap locationMap = _locationMap world

     pure ()

     pure ()
manageAdministration _ = error "Unhandled administrative action"
