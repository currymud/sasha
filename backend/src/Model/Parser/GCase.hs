{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Model.Parser.GCase where
import           Control.Monad.Identity (Identity)
import           Data.Kind              (Type)
import           GHC.Generics           (Generic)

-- Informed by, but not an implementation of, Case Grammer
-- Used by Objects to determine what functions are required to
-- compute function returned to game loop.

type GCase :: Type
data GCase
  = StimulusVerbPhrase StimulusVerbPhraseF

type StimulusVerbPhraseF :: Type
data StimulusVerbPhraseF
  = ImplicitStimulusVerb (Identity ())
