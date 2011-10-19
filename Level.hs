module Level where

import Modifier

data Level = Level { modifiers :: [Modifier]
                   --, feats :: [Feat]
                   } deriving (Show)

level :: Modifier -> Level
level abil = Level { Level.modifiers=[abil]}


levelConPlus1 = modFactory "Constitution +1 (Level)" "Constitution" 1
levelWisPlus1 = modFactory "Wisdom +1 (Level)" "Wisdom" 1