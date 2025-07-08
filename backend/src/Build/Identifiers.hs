module Build.Identifiers where
import           Actions.Percieve.Look          (agentCanSee)
import           Build.Templates.Identification (makeActionGIDsAuto)
$(makeActionGIDsAuto
  [ [| agentCanSee |]
  ])

-- makeVerbValues [| ImplicitStimulusVerb |] [LOOK, SMELL, TASTE]
