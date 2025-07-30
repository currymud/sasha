{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Model.Parser.Composites.Verbs where
import           Data.Kind                         (Type)
import           GHC.Generics                      (Generic)
import           Model.Parser.Atomics.Prepositions (DirectionalStimulusMarker)
import           Model.Parser.Atomics.Verbs        (DirectionalStimulusVerb,
                                                    EdibleConsumptionVerb,
                                                    ImplicitStimulusVerb,
                                                    SomaticAccessVerb)
import           Model.Parser.Composites.Nouns     (DirectionalStimulusNounPhrase,
                                                    EdibleNounPhrase,
                                                    SomaticStimulusNounPhrase)
import           Relude.String.Conversion          (ToText (toText))

type StimulusVerbPhrase :: Type
data StimulusVerbPhrase
  = ImplicitStimulusVerb ImplicitStimulusVerb
  | DirectStimulusVerbPhrase DirectionalStimulusVerb DirectionalStimulusMarker DirectionalStimulusNounPhrase
  | SomaticStimulusVerbPhrase SomaticAccessVerb SomaticStimulusNounPhrase
  deriving stock (Show, Eq, Ord, Generic)

type ConsumptionVerbPhrase :: Type
data ConsumptionVerbPhrase
  = EdibleVerbPhrase EdibleConsumptionVerb EdibleNounPhrase
  deriving stock (Show, Eq, Ord, Generic)
instance ToText ConsumptionVerbPhrase where
  toText (EdibleVerbPhrase verb nounPhrase) =
    toText verb <> " " <> toText nounPhrase

instance ToText StimulusVerbPhrase where
  toText (ImplicitStimulusVerb verb) =
    toText verb
  toText (DirectStimulusVerbPhrase verb marker nounPhrase) =
    toText verb <> " " <> toText marker <> " " <> toText nounPhrase

type Imperative :: Type
data Imperative
  = StimulusVerbPhrase StimulusVerbPhrase
  | ConsumptionVerbPhrase ConsumptionVerbPhrase
  deriving stock (Show, Eq, Ord, Generic)

instance ToText Imperative where
  toText (StimulusVerbPhrase verbPhrase) =
    toText verbPhrase
  toText (ConsumptionVerbPhrase verbPhrase) =
    toText verbPhrase
