module Level where

import Modifier
import Power

data Level = Level { modifiers :: [Modifier]
                   , powers :: [Power]
                   --, feats :: [Feat]
                   } deriving (Show)

level :: [Modifier] -> [Power] -> Level
level x y = Level {Level.modifiers=x, Level.powers=y}


levelConPlus1 = modFactory "Constitution +1 (Level)" "Constitution" 1
levelWisPlus1 = modFactory "Wisdom +1 (Level)" "Wisdom" 1