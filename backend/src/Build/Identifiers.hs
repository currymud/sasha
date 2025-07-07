module Build.Identifiers where
import           Actions.Percieve.Look          (agentCanSee)
import           Build.Templates.Identification (makeActionGIDsAuto)
import           Language.Haskell.TH.Lib        (ExpQ)
import           Language.Haskell.TH.Syntax     (Dec, Q)

$(makeActionGIDsAuto
  [ [| agentCanSee |]
  ])
