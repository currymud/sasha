module Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb
         (isaLook,look,smell,taste,listen,touch,inventory,implicitStimulusVerbs) where

import           Data.HashSet                                (HashSet, fromList)
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Verbs                  (ImplicitStimulusVerb (ImplicitStimulusVerb))
import           Model.Parser.Lexer                          (Lexeme (INVENTORY, LISTEN, LOOK, SMELL, TASTE, TOUCH))


makeSemanticValues [| ImplicitStimulusVerb |] [LOOK, SMELL, TASTE,LISTEN, TOUCH,INVENTORY]

isaLook :: ImplicitStimulusVerb
isaLook = Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb.look

implicitStimulusVerbs :: HashSet ImplicitStimulusVerb
implicitStimulusVerbs =
  fromList [inventory,look,smell,taste,listen,touch]

