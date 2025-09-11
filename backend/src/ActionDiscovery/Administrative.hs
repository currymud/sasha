{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module ActionDiscovery.Administrative (manageAdministration) where

import           Control.Monad.Identity     (Identity)
import           Model.Core                 (GameComputation)

import           Model.Parser.Atomics.Verbs (AdministrativeVerb (AdministrativeVerb))
import           Model.Parser.Lexer         (Lexeme (DEBUG))

manageAdministration :: AdministrativeVerb -> GameComputation Identity ()
manageAdministration (AdministrativeVerb  DEBUG) = pure () -- this constructor is depricated
manageAdministration _ = error "Unhandled administrative action"
