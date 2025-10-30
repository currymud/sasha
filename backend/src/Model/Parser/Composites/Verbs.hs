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
                                                    ContainerAccessVerb,
                                                    DirectionalStimulusVerb,
                                                    ImplicitStimulusVerb,
                                                    NegativePosturalVerb,
                                                    PositivePosturalVerb,
                                                    SimpleAccessVerb,
                                                    SomaticAccessVerb)
import           Model.Parser.Composites.Nouns     (ConsumableNounPhrase,
                                                    ContainerPhrase,
                                                    DirectionalStimulusContainerPhrase,
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

type ContainerAccessVerbPhrase :: Type
data ContainerAccessVerbPhrase
  = SimpleAccessContainerVerbPhrase SimpleAccessVerb ContainerPhrase
  | ContainerAccessVerbPhrase SimpleAccessVerb ContainerPhrase InstrumentalAccessNounPhrase
  deriving stock (Show, Eq, Ord,Generic)

instance ToText ContainerAccessVerbPhrase where
  toText (SimpleAccessContainerVerbPhrase verb containerPhrase) =
    toText verb <> " " <> toText containerPhrase
  toText (ContainerAccessVerbPhrase verb containerPhrase instrumentalNounPhrase) =
    toText verb <> " " <> toText containerPhrase <> " with " <> toText instrumentalNounPhrase

type StimulusVerbPhrase :: Type
data StimulusVerbPhrase
  = ImplicitStimulusVerb ImplicitStimulusVerb
  | DirectStimulusVerbPhrase DirectionalStimulusVerb DirectionalStimulusNounPhrase
  | DirectionalStimulusContainmentPhrase DirectionalStimulusVerb DirectionalStimulusContainerPhrase
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
  = SimplePositivePosturalVerbPhrase PositivePosturalVerb
  | SimpleNegativePosturalVerbPhrase NegativePosturalVerb
  | PositivePosturalVerbPhrase PositivePosturalVerb PositivePosturalDirection      -- "stand up"
  | NegativePosturalVerbPhrase NegativePosturalVerb NegativePosturalDirection -- "sit down"
  deriving stock (Show, Eq, Ord, Generic)

instance ToText PosturalVerbPhrase where
  toText (PositivePosturalVerbPhrase verb direction) =
    toText verb <> " " <> toText direction
  toText (NegativePosturalVerbPhrase verb direction) =
    toText verb <> " " <> toText direction
  toText (SimplePositivePosturalVerbPhrase verb) =
    toText verb
  toText (SimpleNegativePosturalVerbPhrase verb) =
    toText verb
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
  | ContainerAccessVerbPhrase' ContainerAccessVerbPhrase
  | StimulusVerbPhrase StimulusVerbPhrase
  | ConsumptionVerbPhrase' ConsumptionVerbPhrase
  | AcquisitionVerbPhrase' AcquisitionVerbPhrase
  | PosturalVerbPhrase PosturalVerbPhrase
  deriving stock (Show, Eq, Ord, Generic)

instance ToText Imperative where
  toText (Administrative verb) =
    toText verb
  toText (ContainerAccessVerbPhrase' verbPhrase) =
    toText verbPhrase
  toText (StimulusVerbPhrase verbPhrase) =
    toText verbPhrase
  toText (ConsumptionVerbPhrase' verbPhrase) =
    toText verbPhrase
  toText (AcquisitionVerbPhrase' verbPhrase) =
    toText verbPhrase
  toText (PosturalVerbPhrase verbPhrase) =
    toText verbPhrase
