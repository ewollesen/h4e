module CharacterClass where

import Modifier


data Class = Class { name :: String
                   , hpAtFirstLevel :: Int
                   , hpPerLevelGained :: Int
                   , healingSurgesPerDay :: Int
                   , modifiers :: [Modifier]
                   } deriving (Show)

-- is there a way to generate these from a list?
classWisPlus2 = modFactory "+2 Wisdom (Class)" "Wisdom" 2