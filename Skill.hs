module Skill where


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
  skillArmorPenalty :: a -> SkillName -> Int
  trainedSkills :: a -> [SkillName]
  trainedSkill :: a -> SkillName -> Bool

skillHasArmorPenalty Acrobatics = True
skillHasArmorPenalty Athletics = True
skillHasArmorPenalty Endurance = True
skillHasArmorPenalty Stealth = True
skillHasArmorPenalty Thievery = True
skillHasArmorPenalty n = False

skillArmorPenaltyApplies name True
  | skillHasArmorPenalty name == True = True
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
