module Parser.Partitions.Prepositions where

import           Data.Hashable            (Hashable)
import           Data.HashSet             (HashSet, fromList, singleton, toList)
import           Data.Kind                (Type)
import           GHC.Generics             (Generic)
import           Lexer
import           Relude.String.Conversion (ToText)

locationInterrogativeMarker :: HashSet LocationInterrogativeMarker
locationInterrogativeMarker = singleton $ LocationInterrogativeMarker WHERE

objectInterrogativeMarker :: HashSet ObjectInterrogativeMarker
objectInterrogativeMarker = singleton $ ObjectInterrogativeMarker WHAT

traversalMarkers :: HashSet TraversalMarker
traversalMarkers = singleton $ TraversalMarker THROUGH

processingMethods :: HashSet ProcessingMethod
processingMethods = singleton $ ProcessingMethod THROUGH

containmentMarkers :: HashSet ContainmentMarker
containmentMarkers = fromList $ map ContainmentMarker [IN, INTO]

surfaceMarkers :: HashSet SurfaceMarker
surfaceMarkers = fromList $ map SurfaceMarker [ON, ONTO]

recipientMarker :: HashSet RecipientMarker
recipientMarker = fromList $ RecipientMarker <$> [TO]

sourceMarker :: HashSet SourceMarker
sourceMarker = fromList $ SourceMarker <$> [FROM]

instrumentalMarker :: HashSet InstrumentalMarker
instrumentalMarker = Data.HashSet.singleton $ InstrumentalMarker WITH

-- Prepositions that can signify direction
paths :: HashSet Path
paths = fromList
  $ map Path [THROUGH, TO, INTO,ACROSS,OVER
               ,UNDER,AROUND,ONTO, UP,DOWN,BETWEEN, OFF]

directionalStimulusMarkers :: HashSet DirectionalStimulusMarker
directionalStimulusMarkers = singleton $ DirectionalStimulusMarker AT

targetedStimulusMarker :: HashSet TargetedStimulusMarker
targetedStimulusMarker = singleton $ TargetedStimulusMarker TO
-- look on the table

surfaceStimulusMarker :: HashSet SurfaceStimulusMarker
surfaceStimulusMarker = singleton $ SurfaceStimulusMarker ON

containerStimulusMarker :: HashSet ContainerStimulusMarker
containerStimulusMarker = singleton $ ContainerStimulusMarker IN

regionMarker :: HashSet RegionMarker
regionMarker = fromList $ map RegionMarker [IN, ON]

-- Nouns that can be asked about Referential :: Topic
treferentialMarker :: HashSet TReferentialMarker
treferentialMarker = singleton $ TReferentialMarker UP

topicMarker :: HashSet TopicMarker
topicMarker = singleton $ TopicMarker ABOUT

axialMarker :: HashSet AxialMarker
axialMarker = fromList
  $ map AxialMarker [BEHIND, UNDER, ABOVE , FRONT, LEFT, RIGHT]

verticalAxis :: HashSet VerticalAxis
verticalAxis = fromList $ map VerticalAxis [UNDER, ABOVE]

frontBackAxis :: HashSet FrontBackAxis
frontBackAxis = fromList $ map FrontBackAxis [BEHIND, FRONT]

leftRightAxis :: HashSet LeftRightAxis
leftRightAxis = fromList $ map LeftRightAxis [LEFT, RIGHT]

referenceSourceMarkers :: HashSet ReferenceSourceMarker
referenceSourceMarkers = singleton $ ReferenceSourceMarker IN
