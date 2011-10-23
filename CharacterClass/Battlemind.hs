module CharacterClass.Battlemind where

import qualified CharacterClass as CC
import Modifier
import Skill

battlemind :: [Modifier] -> [SkillName] -> CC.Class
battlemind x y = CC.Class { CC.name="battlemind"
                          , CC.hpAtFirstLevel=15
                          , CC.hpPerLevelGained=6
                          , CC.healingSurgesPerDay=9
                          , CC.modifiers=CC.classWillPlus2:x
                          , CC.trainedSkills=y
                          }
