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

skillNames = [Skill.Acrobatics .. Skill.Thievery]

class Skilled a where
  skill :: Skilled a => SkillName -> a -> Int
  skillMods :: Skilled a => SkillName -> a -> [Modifier]
  skillMods s c = [] -- TODO
  skillArmorCheckPenalty :: Skilled a => SkillName -> a -> Int
  skillsTrained :: Skilled a => a -> [SkillName]
  skillTrained :: Skilled a => SkillName -> a -> Bool
  skillAbilModPlusHalfLevel :: Skilled a => SkillName -> a -> Int

skillAbil Skill.Acrobatics = Ability.Dexterity
skillAbil Skill.Arcana = Ability.Intelligence
skillAbil Skill.Athletics = Ability.Strength
skillAbil Skill.Bluff = Ability.Charisma
skillAbil Skill.Diplomacy = Ability.Charisma
skillAbil Skill.Dungeoneering = Ability.Wisdom
skillAbil Skill.Endurance = Ability.Constitution
skillAbil Skill.Heal = Ability.Wisdom
skillAbil Skill.History = Ability.Intelligence
skillAbil Skill.Insight = Ability.Wisdom
skillAbil Skill.Intimidate = Ability.Charisma
skillAbil Skill.Nature = Ability.Wisdom
skillAbil Skill.Perception = Ability.Wisdom
skillAbil Skill.Religion = Ability.Intelligence
skillAbil Skill.Stealth = Ability.Dexterity
skillAbil Skill.Streetwise = Ability.Charisma
skillAbil Skill.Thievery = Ability.Dexterity

skillHasArmorCheckPenalty Skill.Acrobatics = True
skillHasArmorCheckPenalty Skill.Athletics = True
skillHasArmorCheckPenalty Skill.Endurance = True
skillHasArmorCheckPenalty Skill.Stealth = True
skillHasArmorCheckPenalty Skill.Thievery = True
skillHasArmorCheckPenalty n = False

skillArmorCheckPenaltyApplies name True
  | skillHasArmorCheckPenalty name == True = True
  | otherwise = False

$(derive[''SkillName])

skillNameToString :: String -> String
skillNameToString sn = sn

instance Data ToJsonD SkillName => ToJson SkillName where
  toJson = (enumToJson skillNameToString)

instance (Data FromJsonD SkillName, TranslateField SkillName) => FromJson SkillName where
  fromJson = (enumFromJson skillNameToString)

skillNameToModTarget :: SkillName -> ModTarget
skillNameToModTarget Skill.Acrobatics = Modifier.Acrobatics
skillNameToModTarget Skill.Arcana = Modifier.Arcana
skillNameToModTarget Skill.Athletics = Modifier.Athletics
skillNameToModTarget Skill.Bluff = Modifier.Bluff
skillNameToModTarget Skill.Diplomacy = Modifier.Diplomacy
skillNameToModTarget Skill.Dungeoneering = Modifier.Dungeoneering
skillNameToModTarget Skill.Endurance = Modifier.Endurance
skillNameToModTarget Skill.Heal = Modifier.Heal
skillNameToModTarget Skill.History = Modifier.History
skillNameToModTarget Skill.Insight = Modifier.Insight
skillNameToModTarget Skill.Intimidate = Modifier.Intimidate
skillNameToModTarget Skill.Nature = Modifier.Nature
skillNameToModTarget Skill.Perception = Modifier.Perception
skillNameToModTarget Skill.Religion = Modifier.Religion
skillNameToModTarget Skill.Stealth = Modifier.Stealth
skillNameToModTarget Skill.Streetwise = Modifier.Streetwise
skillNameToModTarget Skill.Thievery = Modifier.Thievery
