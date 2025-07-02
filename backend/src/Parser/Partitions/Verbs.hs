module Parser.Partitions.Verbs where

import           Data.Hashable (Hashable)
import           Data.HashSet  (HashSet, fromList, singleton)
import           Data.Kind     (Type)
import           Lexer
import           Relude        (ToText)

copula :: HashSet Copula
copula = singleton $ Copula IS

cardinalMovementVerbs :: HashSet CardinalMovementVerb
cardinalMovementVerbs = fromList
  $ map CardinalMovementVerb [GO, SAIL, SNEAK,RUN , MARCH, FLOAT, FLEE, WALK]

spaceTransitionalVerbs :: HashSet SpaceTransitionalVerb
spaceTransitionalVerbs =
  fromList $ map SpaceTransitionalVerb [CLIMB,CRAWL,SWIM,JUMP]

implicitBoundaryVerbs :: HashSet ImplicitBoundaryVerb
implicitBoundaryVerbs = singleton $ ImplicitBoundaryVerb EXIT

explicitBoundaryVerbs :: HashSet ExplicitBoundaryVerb
explicitBoundaryVerbs = singleton $ ExplicitBoundaryVerb ENTER

implicitRegionalStimulusVerbs :: HashSet ImplicitRegionalStimulusVerb
implicitRegionalStimulusVerbs = fromList $ map ImplicitRegionalStimulusVerb [WAIT, SLEEP]

implicitStimulusVerbs :: HashSet ImplicitStimulusVerb
implicitStimulusVerbs =
  fromList $ map ImplicitStimulusVerb [LOOK, SMELL, TASTE, LISTEN, TOUCH]

explicitStimulusVerbs :: HashSet ExplicitStimulusVerb
explicitStimulusVerbs = fromList $ map ExplicitStimulusVerb [SMELL,TASTE,TOUCH]

-- Verbs that can take directional prepositions like "at"
directionalStimulusVerbs :: HashSet DirectionalStimulusVerb
directionalStimulusVerbs = singleton $ DirectionalStimulusVerb LOOK

-- Verbs that can take targeted prepositions like "to"
targetedStimulusVerbs :: HashSet TargetedStimulusVerb
targetedStimulusVerbs = singleton $ TargetedStimulusVerb LISTEN

traversalVerbs :: HashSet TraversalVerb
traversalVerbs = fromList
  $ map TraversalVerb [MOVE, THROW,TOSS, WALK, PUSH]

traversalPathVerbs :: HashSet TraversalPathVerb
traversalPathVerbs = singleton $ TraversalPathVerb SLIDE

toggleVerbs :: HashSet ToggleVerb
toggleVerbs = fromList $ map ToggleVerb [FLIP, PUSH]

modToggleVerbs :: HashSet ModToggleVerb
modToggleVerbs = singleton $ ModToggleVerb TURN

-- These verbs don't take instruments
simpleAccessVerbs :: HashSet SimpleAccessVerb
simpleAccessVerbs = fromList $ map SimpleAccessVerb [OPEN, CLOSE]

-- These verbs can take instruments (with "with" phrase)
instrumentalAccessVerbs :: HashSet InstrumentalAccessVerb
instrumentalAccessVerbs = fromList $ map InstrumentalAccessVerb [LOCK, UNLOCK]

rotationalVerbs :: HashSet RotationalVerb
rotationalVerbs = fromList $ map RotationalVerb [TURN, TWIST, ROTATE]

directionalVerbs :: HashSet DirectionalVerb
directionalVerbs = fromList $ map DirectionalVerb [PUSH, PULL]

instrumentActionVerbs :: HashSet InstrumentActionVerb
instrumentActionVerbs = fromList $ map InstrumentActionVerb [PLAY, RING, SHOOT]

instrumentalPlacementVerbs :: HashSet InstrumentalPlacementVerb
instrumentalPlacementVerbs = fromList $ map InstrumentalPlacementVerb [PLANT,PUT,INSERT] -- Verbs that require/allow tools

generalPlacementVerbs :: HashSet GeneralPlacementVerb
generalPlacementVerbs = fromList $ map GeneralPlacementVerb [DROP, PUT] -- Verbs with minimal restrictions

acquisitionVerbs :: HashSet AcquisitionVerb
acquisitionVerbs = fromList $ map AcquisitionVerb [GET,REMOVE]

transferVerbs :: HashSet TransferVerb
transferVerbs = singleton $ TransferVerb GIVE

researchVerbs :: HashSet ResearchVerb
researchVerbs = singleton $ ResearchVerb LOOK
