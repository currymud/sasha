{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Model.Parser.Composites.Verbs where
import           Data.Kind                         (Type)
import           GHC.Generics                      (Generic)
import           Model.Parser.Atomics.Adverbs      (NegativePosturalDirection,
                                                    PositivePosturalDirection)
import           Model.Parser.Atomics.Prepositions (InstrumentMarker,
                                                    SourceMarker)
import           Model.Parser.Atomics.Verbs        (AcquisitionVerb,
                                                    AdministrativeVerb,
                                                    ConsumptionVerb,
                                                    DirectionalStimulusVerb,
                                                    ImplicitStimulusVerb,
                                                    NegativePosturalVerb,
                                                    PositivePosturalVerb,
                                                    SimpleAccessVerb,
                                                    SomaticAccessVerb)
import           Model.Parser.Composites.Nouns     (ConsumableNounPhrase,
                                                    ContainerPhrase,
                                                    DirectionalStimulusNounPhrase,
                                                    InstrumentalAccessNounPhrase,
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

type AccessVerbPhrase :: Type
data AccessVerbPhrase
  = SimpleAccessVerbPhrase SimpleAccessVerb ContainerPhrase
  | AccessVerbPhrase SimpleAccessVerb ContainerPhrase InstrumentalAccessNounPhrase
  deriving stock (Show, Eq, Ord,Generic)

instance ToText AccessVerbPhrase where
  toText (SimpleAccessVerbPhrase verb containerPhrase) =
    toText verb <> " " <> toText containerPhrase
  toText (AccessVerbPhrase verb containerPhrase instrumentalNounPhrase) =
    toText verb <> " " <> toText containerPhrase <> " with " <> toText instrumentalNounPhrase

type StimulusVerbPhrase :: Type
data StimulusVerbPhrase
  = ImplicitStimulusVerb ImplicitStimulusVerb
  | DirectStimulusVerbPhrase DirectionalStimulusVerb DirectionalStimulusNounPhrase
  | DirectionalStimulusContainmentPhrase DirectionalStimulusVerb ContainerPhrase
  | SomaticStimulusVerbPhrase SomaticAccessVerb SomaticStimulusNounPhrase
  deriving stock (Show, Eq, Ord, Generic)

type ConsumptionVerbPhrase :: Type
data ConsumptionVerbPhrase
  = ConsumptionVerbPhrase ConsumptionVerb ConsumableNounPhrase
  deriving stock (Show, Eq, Ord, Generic)

instance ToText ConsumptionVerbPhrase where
  toText (ConsumptionVerbPhrase verb nounPhrase) =
    toText verb <> " " <> toText nounPhrase

type PosturalVerbPhrase :: Type
data PosturalVerbPhrase
  = PositivePosturalVerbPhrase PositivePosturalVerb PositivePosturalDirection      -- "stand up"
  | NegativePosturalVerbPhrase NegativePosturalVerb NegativePosturalDirection -- "sit down"
  deriving stock (Show, Eq, Ord, Generic)

instance ToText PosturalVerbPhrase where
  toText (PositivePosturalVerbPhrase verb direction) =
    toText verb <> " " <> toText direction
  toText (NegativePosturalVerbPhrase verb direction) =
    toText verb <> " " <> toText direction

instance ToText StimulusVerbPhrase where
  toText (ImplicitStimulusVerb verb) =
    toText verb
  toText (DirectStimulusVerbPhrase verb nounPhrase) =
    toText verb <> " " <> toText nounPhrase
  toText (DirectionalStimulusContainmentPhrase verb containerPhrase) =
    toText verb <> " " <> toText containerPhrase
  toText (SomaticStimulusVerbPhrase verb nounPhrase) =
    toText verb <> " " <> toText nounPhrase

type Imperative :: Type
data Imperative
  = Administrative AdministrativeVerb
  | AccessVerbPhrase' AccessVerbPhrase
  | StimulusVerbPhrase StimulusVerbPhrase
  | ConsumptionVerbPhrase' ConsumptionVerbPhrase
  | AcquisitionVerbPhrase' AcquisitionVerbPhrase
  | PosturalVerbPhrase PosturalVerbPhrase
  deriving stock (Show, Eq, Ord, Generic)

instance ToText Imperative where
  toText (Administrative verb) =
    toText verb
  toText (AccessVerbPhrase' verbPhrase) =
    toText verbPhrase
  toText (StimulusVerbPhrase verbPhrase) =
    toText verbPhrase
  toText (ConsumptionVerbPhrase' verbPhrase) =
    toText verbPhrase
  toText (AcquisitionVerbPhrase' verbPhrase) =
    toText verbPhrase
  toText (PosturalVerbPhrase verbPhrase) =
    toText verbPhrase
