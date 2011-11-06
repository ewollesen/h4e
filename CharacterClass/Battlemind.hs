module CharacterClass.Battlemind where

import qualified CharacterClass as CC
import Modifier
import Skill
import Weapon
import Power
import Ability


battlemind :: [Modifier] -> [SkillName] -> CC.Class
battlemind x y = CC.Class { CC.name="Battlemind"
                          , CC.hpAtFirstLevel=15
                          , CC.hpPerLevelGained=6
                          , CC.healingSurgesPerDay=9
                          , CC.modifiers=CC.classWillPlus2:x
                          , CC.trainedSkills=y
                          , CC.proficientWithWeaponsTaggedWith=[martialWeaponTag, simpleWeaponTag]
                          , CC.proficientWithArmorTaggedWith=[]
                          }

ironFist = Power { Power.name="Iron Fist"
                 , Power.hit="1[W] + Constitution modifier damage"
                 , Power.attackAbility=Constitution
                 , Power.attackVsDefense="AC"
                 , Power.uses="At-Will"
                 }

bullsStrength = Power { Power.name="Bull's Strength"
                      , Power.hit="1[W] + Constitution modifier damage, and you push the target 1 square"
                      , Power.attackAbility=Constitution
                      , Power.attackVsDefense="AC"
                      , Power.uses="At-Will"
                      }

whirlingDefense = Power { Power.name="Whirling Defense"
                        , Power.hit="1[W] + Constitution modifier damage, and you mark the target until the end of your next turn"
                        , Power.attackAbility=Constitution
                        , Power.attackVsDefense="AC"
                        , Power.uses="At-Will"
                        }
