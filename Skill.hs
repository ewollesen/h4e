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