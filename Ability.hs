module Ability where

data AbilityName = Strength
                 | Dexterity
                 | Constitution
                 | Intelligence
                 | Wisdom
                 | Charisma
               deriving (Show, Eq, Enum)

