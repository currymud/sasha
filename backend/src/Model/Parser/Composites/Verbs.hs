{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Model.Parser.Composites.Verbs where
import           Data.Kind                  (Type)
import           GHC.Generics               (Generic)
import           Model.Parser.Atomics.Verbs (ImplicitStimulusVerb)
import           Relude.String.Conversion   (ToText (toText))

type StimulusVerbPhrase :: Type
data StimulusVerbPhrase
  = ImplicitStimulusVerb ImplicitStimulusVerb
  deriving stock (Show, Eq, Ord, Generic)

instance ToText StimulusVerbPhrase where
  toText (ImplicitStimulusVerb verb) =
    toText verb

type Imperative :: Type
data Imperative
  = StimulusVerbPhrase StimulusVerbPhrase -- "Look" "Listen" "Smell" "Taste" "Touch"
  deriving stock (Show, Eq, Ord, Generic)

instance ToText Imperative where
  toText (StimulusVerbPhrase verbPhrase) =
    toText verbPhrase
