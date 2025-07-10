module Test.Arbitrary.Verbs () where

import qualified Data.HashSet                                         as HS
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb
import           Model.Parser.Atomics.Verbs                           (ImplicitStimulusVerb)
import           Test.QuickCheck

instance Arbitrary ImplicitStimulusVerb where
  arbitrary = elements $ HS.toList implicitStimulusVerbs


