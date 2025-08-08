{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Model.Parser.Composites.Verbs where
import           Data.Kind                         (Type)
import           GHC.Generics                      (Generic)
import           Model.Parser.Atomics.Prepositions (DirectionalStimulusMarker,
                                                    SourceMarker)
import           Model.Parser.Atomics.Verbs        (AcquisitionVerb,
                                                    ConsumptionVerb,
                                                    DirectionalStimulusVerb,
                                                    ImplicitStimulusVerb,
                                                    SomaticAccessVerb)
import           Model.Parser.Composites.Nouns     (ConsumableNounPhrase,
                                                    DirectionalStimulusNounPhrase,
                                                    ObjectPhrase,
                                                    SomaticStimulusNounPhrase,
                                                    SupportPhrase)
import           Relude.String.Conversion          (ToText (toText))

type AcquisitionVerbPhrase :: Type
data AcquisitionVerbPhrase
  = SimpleAcquisitionVerbPhrase AcquisitionVerb ObjectPhrase
  | AcquisitionVerbPhrase
      AcquisitionVerb
      ObjectPhrase
      SourceMarker
      SupportPhrase
  deriving stock (Show, Eq, Ord,Generic)

instance ToText AcquisitionVerbPhrase where
  toText (SimpleAcquisitionVerbPhrase verb nounPhrase) =
    toText verb <> " " <> toText nounPhrase
  toText (AcquisitionVerbPhrase verb nounPhrase marker supportPhrase) =
    toText verb <> " " <> toText nounPhrase <> " " <> toText marker <> " " <> toText supportPhrase

type StimulusVerbPhrase :: Type
data StimulusVerbPhrase
  = ImplicitStimulusVerb ImplicitStimulusVerb
  | DirectStimulusVerbPhrase DirectionalStimulusVerb DirectionalStimulusMarker DirectionalStimulusNounPhrase
  | SomaticStimulusVerbPhrase SomaticAccessVerb SomaticStimulusNounPhrase
  deriving stock (Show, Eq, Ord, Generic)

type ConsumptionVerbPhrase :: Type
data ConsumptionVerbPhrase
  = ConsumptionVerbPhrase ConsumptionVerb ConsumableNounPhrase
  deriving stock (Show, Eq, Ord, Generic)
instance ToText ConsumptionVerbPhrase where
  toText (ConsumptionVerbPhrase verb nounPhrase) =
    toText verb <> " " <> toText nounPhrase

instance ToText StimulusVerbPhrase where
  toText (ImplicitStimulusVerb verb) =
    toText verb
  toText (DirectStimulusVerbPhrase verb marker nounPhrase) =
    toText verb <> " " <> toText marker <> " " <> toText nounPhrase
  toText (SomaticStimulusVerbPhrase verb nounPhrase) =
    toText verb <> " " <> toText nounPhrase

type Imperative :: Type
data Imperative
  = StimulusVerbPhrase StimulusVerbPhrase
  | ConsumptionVerbPhrase' ConsumptionVerbPhrase
  | AcquisitionVerbPhrase' AcquisitionVerbPhrase
  deriving stock (Show, Eq, Ord, Generic)

instance ToText Imperative where
  toText (StimulusVerbPhrase verbPhrase) =
    toText verbPhrase
  toText (ConsumptionVerbPhrase' verbPhrase) =
    toText verbPhrase
  toText (AcquisitionVerbPhrase' verbPhrase) =
    toText verbPhrase
