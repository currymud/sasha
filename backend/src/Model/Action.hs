{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Model.Action (ActionF (ImplicitStimulusF)) where

import           Data.Kind (Type)

type ActionF :: Type -> Type
data ActionF a
  = ImplicitStimulusF (Either a a)
