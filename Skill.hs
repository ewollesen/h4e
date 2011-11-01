{-# OPTIONS_GHC
    -XTemplateHaskell
    -XFlexibleInstances
    -XMultiParamTypeClasses
    -XFlexibleContexts
    -XUndecidableInstances #-}

module Skill where

import Text.RJson
import Data.Generics.SYB.WithClass.Basics
import Data.Generics.SYB.WithClass.Derive
import Modifier
import Ability


data SkillName = Acrobatics
               | Arcana
               | Athletics
               | Bluff
               | Diplomacy
               | Dungeoneering
               | Endurance
               | Heal
               | History
               | Insight
               | Intimidate
               | Nature
               | Perception
               | Religion
               | Stealth
               | Streetwise
               | Thievery
               deriving (Show, Eq, Enum)

skillNames = [Acrobatics .. Thievery]

class Skilled a where
  skill :: a -> SkillName -> Int
  skillMods :: a -> SkillName -> [Modifier]
  skillArmorCheckPenalty :: a -> SkillName -> Int
  trainedSkills :: a -> [SkillName]
  trainedSkill :: a -> SkillName -> Bool

skillHasArmorCheckPenalty Acrobatics = True
skillHasArmorCheckPenalty Athletics = True
skillHasArmorCheckPenalty Endurance = True
skillHasArmorCheckPenalty Stealth = True
skillHasArmorCheckPenalty Thievery = True
skillHasArmorCheckPenalty n = False

skillArmorCheckPenaltyApplies name True
  | skillHasArmorCheckPenalty name == True = True
  | otherwise = False

skillPassiveInsight :: (Skilled c) => c -> Int
skillPassiveInsight c = 10 + skill c Insight
skillPassivePerception :: (Skilled c) => c -> Int
skillPassivePerception c = 10 + skill c Perception

skillTrainingBonus = 5 :: Int

trainedBonus :: (Skilled c) => c -> SkillName -> Int
trainedBonus c s
  | trainedSkill c s == True = skillTrainingBonus
  | otherwise            = 0

skillNameToString :: String -> String
skillNameToString sn = sn

instance Data ToJsonD SkillName => ToJson SkillName where
  toJson = (enumToJson skillNameToString)

instance (Data FromJsonD SkillName, TranslateField SkillName) => FromJson SkillName where
  fromJson = (enumFromJson skillNameToString)

$(derive[''SkillName])
