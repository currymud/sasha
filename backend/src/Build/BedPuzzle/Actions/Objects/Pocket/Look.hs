module Build.BedPuzzle.Actions.Objects.Pocket.Look (whatPocket,notEvenPocket,seePocketChair,seePocketRobeWorn, emptyPocket) where
import           Control.Monad.Identity (Identity)
import           Data.Text              (Text)
import           GameState              (modifyNarration)
import           Model.GameState        (DirectionalStimulusActionF (DirectionalStimulusActionF),
                                         GameComputation,
                                         updateActionConsequence)

whatPocket :: DirectionalStimulusActionF
whatPocket = DirectionalStimulusActionF (const (const whatPocket'))
  where
    whatPocket' ::GameComputation Identity ()
    whatPocket'  = modifyNarration $ updateActionConsequence msg

    msg :: Text
    msg = "Pocket? What Pocket?"

notEvenPocket :: DirectionalStimulusActionF
notEvenPocket = DirectionalStimulusActionF (const (const notEvenPocket'))
  where
    notEvenPocket' :: GameComputation Identity ()
    notEvenPocket' = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "One thing at a time. You've just woken up and your eyes are all bleary unfocused and closed. Maybe open them up and go from there?"

seePocketChair :: DirectionalStimulusActionF
seePocketChair = DirectionalStimulusActionF (const (const seePocket'))
  where
    seePocket' :: GameComputation Identity ()
    seePocket' = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You zero in on the pocket of your ratty bathrobe. It's a big pocket, and you can put a lot of stuff in it. You could probably fit a whole pillow in there if you wanted to. You have a vague recollection of an aspirin pill in that pocket"

seePocketRobeWorn :: DirectionalStimulusActionF
seePocketRobeWorn = DirectionalStimulusActionF (const (const seeRobeWorn'))
  where
    seeRobeWorn' :: GameComputation Identity ()
    seeRobeWorn' = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "Eventually we'll have a calculation of what's in the pocket and wheather oyu can see it or not. For now, there's a pill in there you should take."


emptyPocket :: DirectionalStimulusActionF
emptyPocket = DirectionalStimulusActionF (const (const emptyPocket'))
  where
    emptyPocket' :: GameComputation Identity ()
    emptyPocket' = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "The pocket is empty"
