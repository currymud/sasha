module Test.Parser.SpeechParts.Atomics.Nouns where
import           Parser.SpeechParts.Atomics.Nouns      (agents, containers,
                                                        directionalStimulii,
                                                        instrumentalAccessNouns,
                                                        instrumentals,
                                                        lreferentials,
                                                        modToggleNouns,
                                                        namedAgents,
                                                        objectPaths, objectives,
                                                        processableDevices,
                                                        referenceMaterials,
                                                        regions,
                                                        simpleAccessNouns,
                                                        supportives, surfaces,
                                                        switches,
                                                        targetedStimulii,
                                                        toggleNouns,
                                                        treferentials)
import           Test.Hspec                            (Spec, describe, hspec)
import           Test.Hspec.QuickCheck                 (prop)
import           Test.Parser.SpeechParts.Atomics.Utils (checkLexeme)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Nouns Arbitrary" do
  prop "Objective" $ checkLexeme objectives
  prop "Supportive" $ checkLexeme supportives
  prop "Container" $ checkLexeme containers
  prop "Surface" $ checkLexeme surfaces
  prop "ToggleNoun" $ checkLexeme toggleNouns
  prop "ModToggleNoun" $ checkLexeme modToggleNouns
  prop "SimpleAccessNoun" $ checkLexeme simpleAccessNouns
  prop "InstrumentalAccessNoun" $ checkLexeme instrumentalAccessNouns
  prop "Switch" $ checkLexeme switches
  prop "Instrumental" $ checkLexeme instrumentals
  prop "ProcessableDevice" $ checkLexeme processableDevices
  prop "ObjectPath" $ checkLexeme objectPaths
  prop "LReferentials" $ checkLexeme lreferentials
  prop "DirectionalStimulus" $ checkLexeme directionalStimulii
  prop "TargetedStimulus" $ checkLexeme targetedStimulii
  prop "Region" $ checkLexeme regions
  prop "TReferentials" $ checkLexeme treferentials
  prop "Agent" $ checkLexeme agents
  prop "NamedAgent" $ checkLexeme namedAgents
  prop "ReferenceMaterial" $ checkLexeme referenceMaterials



