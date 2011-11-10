module Pontus where

import qualified CharacterClass as CC
import Modifier
import Character
import Race
import CharacterClass
import CharacterClass.Battlemind
import Level
import Skill
import Equipment
import Weapon
import Feat


pontus = Character { Character.name = "Pontus"
                   , playerName = "Eric Wollesen"
                   , baseStr=11
                   , baseCon=17
                   , baseDex=10
                   , baseInt=10
                   , baseWis=14
                   , baseCha=12
                   , characterClass=battlemind [] [Skill.Heal,
                                                   Skill.Diplomacy,
                                                   Skill.Endurance,
                                                   Skill.Insight]
                   , race=human Wisdom
                   , levels=[
                     Level.level [] [bullsStrength, ironFist, whirlingDefense, aspectOfElevatedHarmony, battlemindsDemand, battleResilience, blurredStep, mindSpike] [deceptiveMind, lureOfIron],
                     Level.level [] [dimensionSlide] [psionicToughness]
                     ]
                   , gear=[scaleMail, lightShield]
                   , weapons=[longsword, dagger]
                   , xp=1675
                   , age=24
                   , gender="Male"
                   , height="5' 9\""
                   , weight="150 lbs"
                   , alignment="Unaligned"
                   , deity=""
                   , languages=["Common"]
                   , adventuringCompanyOrOtherAffiliations=""
                   }
