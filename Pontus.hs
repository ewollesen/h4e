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


battleaxe1 = weaponFactory "Battleaxe +1"
             [modFactory "+1 Attack bonus (Enhancement)" Modifier.Attack 1 EnhancementMod,
             modFactory "+1 Damage bonus (Enhancement)" Damage 1 EnhancementMod]
             [weaponTag, martialWeaponTag, meleeWeaponTag]
             2

chainmail1Dwarven = equipFactory "Chain mail +1 (Dwarven)"
            [modFactory "+6 AC bonus (Armor)" ArmorClass 6 ArmorMod,
             modFactory "+1 AC bonus (Enhancment)" ArmorClass 1 EnhancementMod,
             modFactory "-1 Skill penalty (Armor)" Skill (-1) ArmorMod,
             modFactory "-1 Speed penalty (Armor)" Speed (-1) ArmorMod]
            [heavyArmorTag, armorTag]

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
                   , gear=[chainmail1Dwarven, lightShield]
                   , weapons=[battleaxe1, dagger]
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
