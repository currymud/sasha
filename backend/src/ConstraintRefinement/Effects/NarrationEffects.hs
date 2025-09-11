{-# LANGUAGE OverloadedStrings #-}

module ConstraintRefinment.Effects.NarrationEffects where

import           Data.Text           (Text)
import           Model.Core          (Effect,
                                      NarrationOperation (ActionConsequenceNarration, PlayerActionNarration))
import           Model.EDSL.SashaDSL (SashaDSL,
                                      createActionConsequenceNarration,
                                      createPlayerActionNarration)

-- | Create a narration effect for player actions
playerActionNarration :: Text -> SashaDSL Effect
playerActionNarration = createPlayerActionNarration

-- | Create a narration effect for action consequences
actionConsequenceNarration :: Text -> SashaDSL Effect
actionConsequenceNarration = createActionConsequenceNarration

-- Common narration effects for various game situations

-- Look-related narrations
pitchBlackNarration :: SashaDSL Effect
pitchBlackNarration = actionConsequenceNarration "It's pitch black. You can't see a thing."

emptyContainerNarration :: SashaDSL Effect
emptyContainerNarration = actionConsequenceNarration "It's empty."

-- Error/denial narrations
notHereNarration :: SashaDSL Effect
notHereNarration = actionConsequenceNarration "That's not here. Try something else."

cantDoThatNarration :: SashaDSL Effect
cantDoThatNarration = actionConsequenceNarration "You can't do that right now."

-- Inventory narrations
emptyInventoryNarration :: SashaDSL Effect
emptyInventoryNarration = actionConsequenceNarration "You've got nothing but a terrible headache and a slight pang of regret."

-- Object-specific narrations for SashaDemo
eyesClosedRobeNarration :: SashaDSL Effect
eyesClosedRobeNarration = actionConsequenceNarration
  "One thing at a time. You've just woken up and your eyes are all bleary unfocused and closed. Maybe open them up and go from there?"

eyesClosedFloorNarration :: SashaDSL Effect
eyesClosedFloorNarration = actionConsequenceNarration
  "It's too dark. You can't see the floor. Maybe opening your eyes would help?"

eyesClosedPillNarration :: SashaDSL Effect
eyesClosedPillNarration = actionConsequenceNarration
  "Open eyes. Look around. Find pill. This is the algorithm. Let's start at the beginning: Open eyes."

eyesClosedMailNarration :: SashaDSL Effect
eyesClosedMailNarration = actionConsequenceNarration
  "The mail? You want to look at mail now? Your eyes are closed and you can barely think straight."

eyesClosedChairNarration :: SashaDSL Effect
eyesClosedChairNarration = actionConsequenceNarration
  "Eyes closed equals no visual input. Please open your optical sensors."

eyesClosedPocketNarration :: SashaDSL Effect
eyesClosedPocketNarration = actionConsequenceNarration
  "That seems premature. Open your eyes first."

eyesClosedTableNarration :: SashaDSL Effect
eyesClosedTableNarration = actionConsequenceNarration
  "You need to open your eyes first before looking at furniture."

-- Get/take denial narrations
getRobeDeniedNarration :: SashaDSL Effect
getRobeDeniedNarration = actionConsequenceNarration
  "The chair looks a bit far away. You should get up first."

getPillDeniedNarration :: SashaDSL Effect
getPillDeniedNarration = actionConsequenceNarration
  "You try but feel dizzy and have to lay back down"

getFloorDeniedNarration :: SashaDSL Effect
getFloorDeniedNarration = actionConsequenceNarration
  "You can't get the floor."

getMailDeniedNarration :: SashaDSL Effect
getMailDeniedNarration = actionConsequenceNarration
  "You can't reach the mail from here."

-- Stand-related narrations
standDeniedNarration :: SashaDSL Effect
standDeniedNarration = actionConsequenceNarration
  "You open your eyes more, squinting. The room blurs less. You try to stand up but -- nope. Dizzy."

standSuccessNarration :: SashaDSL Effect
standSuccessNarration = actionConsequenceNarration
  "You get up. Slowly. But successfully. The dizziness passes."

-- Open-related narrations
openEyesDeniedNarration :: SashaDSL Effect
openEyesDeniedNarration = actionConsequenceNarration
  "You blink groggily trying to open them more but the light burns your retinas"

openEyesSuccessNarration :: SashaDSL Effect
openEyesSuccessNarration = actionConsequenceNarration
  "You open your eyes a bit. It's bright. Too bright. But you can make out shapes."

pocketOutOfReachNarration :: SashaDSL Effect
pocketOutOfReachNarration = actionConsequenceNarration
  "The robe is on the chair. You'd need to get it first."

pocketClosedNarration :: SashaDSL Effect
pocketClosedNarration = actionConsequenceNarration
  "You should open the pocket first."

-- Item possession narrations
alreadyHaveRobeNarration :: SashaDSL Effect
alreadyHaveRobeNarration = actionConsequenceNarration
  "You already have the robe."

alreadyHaveMailNarration :: SashaDSL Effect
alreadyHaveMailNarration = actionConsequenceNarration
  "You already have the mail."

alreadyHavePillNarration :: SashaDSL Effect
alreadyHavePillNarration = actionConsequenceNarration
  "You already have the pill in your inventory."

-- Take action narrations
takeDeniedNarration :: SashaDSL Effect
takeDeniedNarration = actionConsequenceNarration
  "You can't take that."

-- Generic pickup narration
pickUpNarration :: SashaDSL Effect
pickUpNarration = actionConsequenceNarration "You pick it up."

-- Dynamic narration builders for runtime text
dynamicActionConsequence :: Text -> SashaDSL Effect
dynamicActionConsequence = actionConsequenceNarration

dynamicPlayerAction :: Text -> SashaDSL Effect
dynamicPlayerAction = playerActionNarration
