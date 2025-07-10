module Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb
         (look,smell,taste,listen,touch,implicitStimulusVerbs) where

import           Data.HashSet                                (HashSet, fromList)
import           Grammar.Parser.Partitions.Templates.Atomics (makeVerbValues)
import           Model.Parser.Atomics.Verbs                  (ImplicitStimulusVerb (ImplicitStimulusVerb))
import           Model.Parser.Lexer                          (Lexeme (LISTEN, LOOK, SMELL, TASTE, TOUCH))


makeVerbValues [| ImplicitStimulusVerb |] [LOOK, SMELL, TASTE,LISTEN, TOUCH]
implicitStimulusVerbs :: HashSet ImplicitStimulusVerb
implicitStimulusVerbs =
  fromList $ [look,smell,taste,listen,touch]

