module CharacterClass where

import Modifier
import Skill

data Class = Class { name :: String
                   , hpAtFirstLevel :: Int
                   , hpPerLevelGained :: Int
                   , healingSurgesPerDay :: Int
                   , trainedSkills :: [SkillName]
                   , modifiers :: [Modifier]
                   } deriving (Show)

-- is there a way to generate these from a list?
classWisPlus2 = modFactory "+2 Wisdom (Class)" "Wisdom" 2
classWillPlus2 = modFactory "+2 Will Defense (Class)" "Will" 2