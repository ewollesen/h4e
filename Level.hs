module Level where

import Modifier
import Power
import Feat

data Level = Level { modifiers :: [Modifier]
                   , powers :: [Power]
                   , feats :: [Feat]
                   } deriving (Show)

level :: [Modifier] -> [Power] -> [Feat] -> Level
level x y z = Level {Level.modifiers=x, Level.powers=y, Level.feats=z}


levelConPlus1 = modFactory "Constitution +1 (Level)" "Constitution" 1
levelWisPlus1 = modFactory "Wisdom +1 (Level)" "Wisdom" 1