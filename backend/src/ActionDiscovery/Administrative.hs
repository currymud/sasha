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
import           Debug.Trace                (trace)
import           Grammar.Parser.Lexer       (Lexeme (DEBUG))
import           Model.Core.Mappings        (GIDToDataMap (GIDToDataMap))
import           Model.Parser.Atomics.Verbs (AdministrativeVerb (AdministrativeVerb))

manageAdministration :: AdministrativeVerb -> GameComputation Identity ()
manageAdministration (AdministrativeVerb  DEBUG) = do
     modifyNarration $ updateActionConsequence "=== GAME STATE DUMP ==="

      -- Dump effect registry
     effectRegistry <- gets _effectRegistry

     trace ("Effect Registry: " <> show effectRegistry) $ pure ()

      -- Dump player info
     player <- getPlayerM
     trace ("Player: " <> show player) $ pure ()

      -- Dump world info
     world <- gets _world
     let GIDToDataMap objectMap = _objectMap world
         GIDToDataMap locationMap = _locationMap world

     trace ("Object Map Keys: " <> (show (Data.Map.Strict.keys objectMap))) $ pure ()
     trace ("Location Map Keys: " <> (show (Data.Map.Strict.keys locationMap))) $ pure ()

     trace "=== END GAME STATE DUMP ===" $ pure ()

     pure ()
manageAdministration _ = error "Unhandled administrative action"
