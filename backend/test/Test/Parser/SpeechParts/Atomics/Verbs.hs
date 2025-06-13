module Test.Parser.SpeechParts.Atomics.Verbs where
import           Data.HashSet                     (HashSet, toList)
import           Data.Kind                        (Type)
import           Parser.SpeechParts.Atomics.Verbs (acquisitionVerbs,
                                                   cardinalMovementVerbs,
                                                   copula,
                                                   directionalStimulusVerbs,
                                                   directionalVerbs,
                                                   explicitBoundaryVerbs,
                                                   explicitStimulusVerbs,
                                                   generalPlacementVerbs,
                                                   implicitBoundaryVerbs,
                                                   implicitRegionalStimulusVerbs,
                                                   implicitStimulusVerbs,
                                                   instrumentActionVerbs,
                                                   instrumentalAccessVerbs,
                                                   instrumentalPlacementVerbs,
                                                   modToggleVerbs,
                                                   researchVerbs,
                                                   rotationalVerbs,
                                                   simpleAccessVerbs,
                                                   spaceTransitionalVerbs,
                                                   targetedStimulusVerbs,
                                                   toggleVerbs, transferVerbs,
                                                   traversalPathVerbs,
                                                   traversalVerbs)
import           Test.Hspec                       (Spec, describe, hspec)
import           Test.Hspec.QuickCheck            (prop)
import           Test.QuickCheck                  (Testable (property))
import           Test.QuickCheck.Arbitrary        (Arbitrary)
import           Test.QuickCheck.Property         (Property)

checkLexeme :: forall (a :: Type).
  ( Arbitrary a, Show a, Eq a) => HashSet a -> Property
checkLexeme lexemes = property checkLexeme'
  where
    xs = toList lexemes
    checkLexeme' ::  a -> Bool
    checkLexeme' lexeme = lexeme `elem` xs

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Lexeme Arbitrary" do
  prop "Copula" $ checkLexeme copula
  prop "CardinalMovementVerb" $ checkLexeme cardinalMovementVerbs
  prop "SpaceTransitionalVerb" $ checkLexeme spaceTransitionalVerbs
  prop "ImplicitBoundaryVerb" $ checkLexeme implicitBoundaryVerbs
  prop "ExplicitBoundaryVerb" $ checkLexeme explicitBoundaryVerbs
  prop "ImplicitRegionalStimulusVerb" $ checkLexeme implicitRegionalStimulusVerbs
  prop "ImplicitStimulusVerb" $ checkLexeme implicitStimulusVerbs
  prop "ExplicitStimulusVerb" $ checkLexeme explicitStimulusVerbs
  prop "DirectionalStimulusVerb" $ checkLexeme directionalStimulusVerbs
  prop "TargetedStimulusVerb" $ checkLexeme targetedStimulusVerbs
  prop "TraversalVerb" $ checkLexeme traversalVerbs
  prop "TraversalPathVerb" $ checkLexeme traversalPathVerbs
  prop "ToggleVerb" $ checkLexeme toggleVerbs
  prop "ModToggleVerb" $ checkLexeme modToggleVerbs
  prop "SimpleAccessVerb" $ checkLexeme simpleAccessVerbs
  prop "InstrumentalAccessVerb" $ checkLexeme instrumentalAccessVerbs
  prop "RotationalVerb" $ checkLexeme rotationalVerbs
  prop "DirectionalVerb" $ checkLexeme directionalVerbs
  prop "InstrumentActionVerb" $ checkLexeme instrumentActionVerbs
  prop "InstrumentalPlacementVerb" $ checkLexeme instrumentalPlacementVerbs
  prop "GeneralPlacementVerb" $ checkLexeme generalPlacementVerbs
  prop "AcquistionVerb" $ checkLexeme acquisitionVerbs
  prop "TransferVerb" $ checkLexeme transferVerbs
  prop "ResearchVerb" $ checkLexeme researchVerbs
  {-
spec :: Spec
spec = describe "JSON round trip" do
  prop "CreateUser" $ checkJSON @CreateUser
-}
