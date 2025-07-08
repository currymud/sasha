module Build.Identifiers where
import           Actions.Percieve.Look          (agentCanSee)
import           Build.Templates.Identification (makeActionGIDsAuto,
                                                 makeVerbValues)
import           Language.Haskell.TH.Lib        (ExpQ)
import           Language.Haskell.TH.Syntax     (Dec, Q)
import           Model.Parser.Atomics.Verbs     (ImplicitStimulusVerb)
import           Model.Parser.Lexer             (Lexeme (LOOK, SMELL, TASTE))
$(makeActionGIDsAuto
  [ [| agentCanSee |]
  ])

makeVerbValues ''ImplicitStimulusVerb [LOOK, SMELL, TASTE]
