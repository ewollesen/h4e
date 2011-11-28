module Pontus4 where

import qualified CharacterClass as CC
import Modifier
import Character
import Race
import CharacterClass
import CharacterClass.Battlemind
import Level
import Skill
import Equipment
import Feat
import Fdf


battleaxe1 = equipFactory "Battleaxe +1"
             [modFactory "+1 Attack bonus (Enhancement)" Modifier.Attack 1 EnhancementMod,
             modFactory "+1 Damage bonus (Enhancement)" Damage 1 EnhancementMod]
             [weaponTag, martialWeaponTag, meleeWeaponTag, magicItemTag]
             (Just 2)

chainmail1Dwarven = equipFactory "Chain mail +1 (Dwarven)"
            [modFactory "+6 AC bonus (Armor)" ArmorClass 6 ArmorMod,
             modFactory "+1 AC bonus (Enhancment)" ArmorClass 1 EnhancementMod,
             modFactory "-1 Skill penalty (Armor)" Skill (-1) ArmorMod,
             modFactory "-1 Speed penalty (Armor)" Speed (-1) ArmorMod,
             modFactory "+1 Endurance (Enhancement)" Modifier.Endurance 1 ItemMod]
            [heavyArmorTag, armorTag, magicItemTag]
            Nothing

amuletOfHealth1 = equipFactory "Amulet of Health +1"
                  [modFactory "+1 Fort bonus (Enhancment)" Fortitude 1 EnhancementMod,
                   modFactory "+1 Reflex bonus (Enhancment)" Reflex 1 EnhancementMod,
                   modFactory "+1 Will bonus (Enhancment)" Will 1 EnhancementMod,
                   modFactory "5 Poison" Resistance 5 UntypedMod]
                  [magicItemTag, neckTag]
                  Nothing

potionOfHealing = equipFactory "Potion of Healing"
                  []
                  [magicItemTag]
                  Nothing

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
                     Level.level [] [dimensionSlide] [psionicToughness],
                     Level.level [] [lodestoneLure] [],
                     Level.level [plus1Wis, plus1Con] [] [resilientDemand]
                     ]
                   , gear=[chainmail1Dwarven, lightShield, battleaxe1, dagger, advKit, amuletOfHealth1, potionOfHealing]
                   , xp=4124
                   , age=24
                   , gender="Male"
                   , height="5' 9\""
                   , weight="150 lbs"
                   , alignment="Unaligned"
                   , deity=""
                   , languages=["Common"]
                   , adventuringCompanyOrOtherAffiliations=""
                   , coinAndOtherWealth="19 GP"
                   }

plus1Con = modFactory "+1 Con (Level)" Constitution 1 UntypedMod
plus1Wis = modFactory "+1 Wis (Level)" Wisdom 1 UntypedMod

main = do
  printFdf pontus
