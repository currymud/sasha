module ConstraintRefinement.Actions.Player.Open where
import           Control.Monad.Error.Class  (throwError)
import           Control.Monad.Identity     (Identity)
import qualified Data.Map.Strict
import qualified Data.Set
import           GameState                  (getPlayerLocationM)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ActionEffectKey,
                                             AgentContainerAccessActionF (AgentCanAccessF, AgentCannotAccessF),
                                             GameComputation,
                                             Location (_objectSemanticMap),
                                             Object, SimpleAccessSearchStrategy,
                                             SomaticAccessActionF (CannotSomaticAccessF, PlayerSomaticAccessActionF))
import           Model.GID                  (GID)
import           Model.Parser.GCase         (NounKey)

openEyesDenied :: SomaticAccessActionF
openEyesDenied = CannotSomaticAccessF denied
 where
   denied :: ActionEffectKey -> GameComputation Identity ()
   denied actionEffectKey = do
     processEffectsFromRegistry actionEffectKey

openEyes :: SomaticAccessActionF
openEyes = PlayerSomaticAccessActionF opened
 where
   opened :: ActionEffectKey
             -> GameComputation Identity ()
   opened actionEffectKey = do
     processEffectsFromRegistry actionEffectKey

openContainerF :: AgentContainerAccessActionF
openContainerF = AgentCanAccessF processEffectsFromRegistry

openContainerDeniedF :: AgentContainerAccessActionF
openContainerDeniedF = AgentCannotAccessF processEffectsFromRegistry

objectSearchStrategy :: SimpleAccessSearchStrategy
objectSearchStrategy nounkey = do
  objectSemanticMap <- _objectSemanticMap <$> getPlayerLocationM
  case Data.Map.Strict.lookup nounkey objectSemanticMap of
    Just objSet
      | not (Data.Set.null objSet) -> pure $ Just (Data.Set.elemAt 0 objSet)
    _ -> pure Nothing

validateObjectSearch :: SimpleAccessSearchStrategy
                          -> NounKey
                          -> GameComputation Identity (GID Object)
validateObjectSearch searchStrategy nounKey = do
  maybeResult <- searchStrategy nounKey
  case maybeResult of
    Nothing        -> throwError "You don't see that here."
    Just objectGID -> pure objectGID
