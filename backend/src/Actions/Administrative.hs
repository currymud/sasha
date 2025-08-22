{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module Actions.Administrative (manageAdministration) where

import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader.Class    (asks)
import qualified Data.Map.Strict
import qualified Data.Set
import           GameState                     (getLocationObjectIDsM,
                                                getPlayerLocationM, getPlayerM,
                                                modifyNarration,
                                                parseConsumptionPhrase)
import           Model.GameState               (ActionEffectKey (LocationKey, PlayerKey),
                                                ActionEffectMap (ActionEffectMap),
                                                ActionKey (ConsumptionActionKey),
                                                ActionMaps (_consumptionActionMap),
                                                Config (_actionMaps),
                                                ConsumptionActionF (ConsumptionActionF),
                                                GameComputation,
                                                GameState (_effectRegistry, _world),
                                                Location (_objectSemanticMap),
                                                Player (_location, _playerActions),
                                                World (..),
                                                updateActionConsequence)

import           Control.Monad.State           (gets)
import qualified Data.Text
import           Debug.Trace                   (trace)
import           GameState.ActionManagement    (lookupConsumption,
                                                processEffectsFromRegistry)
import           GameState.EffectRegistry      (lookupActionEffectsInRegistry)
import           Grammar.Parser.Lexer          (Lexeme (DEBUG, QUIT))
import           Model.GameState.Mappings      (GIDToDataMap (GIDToDataMap))
import           Model.Parser.Atomics.Verbs    (AdministrativeVerb (AdministrativeVerb))
import           Model.Parser.Composites.Verbs (ConsumptionVerbPhrase)

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
