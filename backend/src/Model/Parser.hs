{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Model.Parser where

import           Data.Kind                     (Type)
import           Model.Parser.Composites.Verbs (Imperative)

type Sentence :: Type
data Sentence
  = Imperative Imperative
  deriving stock (Eq,Ord,Show)
