module Level where

import Modifier
import Power
import Feat

data Level = Level { modifiers :: [Modifier]
                   , powers :: [Power]
                   , feats :: [Feat]
                   } deriving (Show)

level :: [Modifier] -> [Power] -> [Feat.Feat] -> Level
level x y z = Level {Level.modifiers=x, Level.powers=y, Level.feats=z}

acMods l = modsByTarget l "AC" -- work down to ac mods from feats gained at each level

levelConPlus1 = modFactory "Constitution +1 (Level)" "Constitution" 1 UntypedMod
levelWisPlus1 = modFactory "Wisdom +1 (Level)" "Wisdom" 1 UntypedMod

instance Modifiable Level where
  modifiers l = Level.modifiers l
