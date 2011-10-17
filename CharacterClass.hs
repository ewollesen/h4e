module CharacterClass where

import Modifier

data Class = Class { name :: String
                   , hpAtFirstLevel :: Int
                   , hpPerLevelGained :: Int
                   , healingSurgesPerDay :: Int
                   , modifiers :: [Modifier]
                   } deriving (Show)

       
       