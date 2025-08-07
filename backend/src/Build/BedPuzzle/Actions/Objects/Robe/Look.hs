module Build.BedPuzzle.Actions.Objects.Robe.Look (whatRobe,notEvenRobe,seeRobeChair,seeRobeWorn) where
import           Control.Monad.Identity (Identity)
import           Data.Text              (Text)
import           GameState              (modifyNarration)
import           Model.GameState        (DirectionalStimulusActionF (DirectionalStimulusActionF),
                                         GameComputation,
                                         updateActionConsequence)

whatRobe :: DirectionalStimulusActionF
whatRobe = DirectionalStimulusActionF (const (const whatPill'))
  where
    whatPill' ::GameComputation Identity ()
    whatPill'  = modifyNarration $ updateActionConsequence msg

    msg :: Text
    msg = "You don't see you're robe here. What are you wearing, exactly?"

notEvenRobe :: DirectionalStimulusActionF
notEvenRobe = DirectionalStimulusActionF (const (const notEvenPill'))
  where
    notEvenPill' :: GameComputation Identity ()
    notEvenPill' = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "One thing at a time. You've just woken up and your eyes are all bleary unfocused and closed. Maybe open them up and go from there?"

seeRobeChair :: DirectionalStimulusActionF
seeRobeChair = DirectionalStimulusActionF (const (const seeRobe'))
  where
    seeRobe' :: GameComputation Identity ()
    seeRobe' = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "The robe is on the chair. It's the only thing you can pick up in your current dizzy state."

seeRobeWorn :: DirectionalStimulusActionF
seeRobeWorn = DirectionalStimulusActionF (const (const seeRobeWorn'))
  where
    seeRobeWorn' :: GameComputation Identity ()
    seeRobeWorn' = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You're wearing your ratty bathrobe. It's got a pocket you can put a ridiulous amount of stuff in. There's something in it."
