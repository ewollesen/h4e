module Skill where

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