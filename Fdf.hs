module Fdf where

import Text.Printf
import Skill
import Character as C
import Pontus


f0 :: Int -> String
f0 i
  | i /= 0 = printf "%d" i
  | otherwise = ""

printFdfHeader = do
  putStrLn "%FDF-1.2"
  putStrLn ""
  putStrLn "1 0 obj"
  putStrLn "<<"
  putStrLn "/FDF << /Fields 2 0 R>>"
  putStrLn ">>"
  putStrLn "endobj"
  putStrLn "2 0 obj"
  putStrLn "["

printFdfFooter = do
  putStrLn "]"
  putStrLn "endobj"
  putStrLn "trailer"
  putStrLn "<<"
  putStrLn "/Root 1 0 R"
  putStrLn ""
  putStrLn ">>"
  putStrLn "%%EOF"

-- Consider a function that if the resulting int is 0, outputs nothing, so
-- that we can write in values on the printed sheet.
printFdfData c = do
  printf "<</T(1 / 2 Level)/V(%d)>>\n" $ halfLevel c
  printf "<</T(10 + 1 / 2 Level)/V(%d)>>\n" $ tenPlusHalfLevel c
  printf "<</T(AC)/V(%d)>>\n" $ ac c
  printf "<</T(AC \\(Armor / Ability\\))/V(%s)>>\n" $ f0 $ acArmorAbility c
  printf "<</T(AC \\(Class\\))/V(%s)>>\n" $ f0 $ acClass c
  printf "<</T(AC \\(Enhancement\\))/V(%s)>>\n" $ f0 $ acEnh c
  printf "<</T(AC \\(Feat\\))/V(%s)>>\n" $ f0 $ acFeat c
  printf "<</T(AC \\(Misc 1\\))/V(%s)>>\n" $ f0 $ acMisc1 c
  printf "<</T(AC \\(Misc 2\\))/V(%s)>>\n" $ f0 $ acMisc2 c
  printf "<</T(Acrobatics)/V(%d)>>\n" $ acrobatics c
  printf "<</T(Acrobatics \\(Abil Mod + 1 / 2 Level\\))/V(%s)>>\n" $ f0 $ acrobaticsAbil c
  printf "<</T(Arcobatics \\(Misc\\))/V(%s)>>\n" $ f0 $ acrobaticsMisc c -- field does not exist?? Typo! Arcobatics
  printf "<</T(Acrobatics \\(Trained\\))/V/%s>>\n" $ acrobaticsTrained c
  printf "<</T(Age)/V(%d)>>\n" $ age c
  printf "<</T(Alignment)/V(%s)>>\n" $ alignment c
  printf "<</T(Arcana)/V(%d)>>\n" $ arcana c
  printf "<</T(Arcana \\(Abil Mod + 1 / 2 Level\\))/V(%s)>>\n" $ f0 $ arcanaAbil c
  printf "<</T(Arcana \\(Misc\\))/V(%s)>>\n" $ f0 $ arcanaMisc c
  printf "<</T(Arcana \\(Trained\\))/V/%s>>\n" $ arcanaTrained c
  mapM (\x -> printf "<</T(At-Will Powers %d)/V(%s)>>\n" x $ atWillPower (x-1) c) [1..6]
  printf "<</T(Athletics)/V(%d)>>\n" $ arcana c
  printf "<</T(Athletics \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>\n" $ athleticsAbil c
  printf "<</T(Athletics \\(Misc\\))/V(%s)>>\n" $ f0 $ athleticsMisc c
  printf "<</T(Athletics \\(Trained\\))/V/%s>>\n" $ athleticsTrained c
  printf "<</T(Attack 1 \\(Abil\\))/V(%s)>>\n" $ f0 $ attack1Abil c
  printf "<</T(Attack 1 \\(Ability\\))/V(%s)>>\n" $ attack1Name c
  printf "<</T(Attack 1 \\(Att Bonus\\))/V(%s)>>\n" $ f0 $ attack1Bonus c
  printf "<</T(Attack 1 \\(Class\\))/V(%s)>>\n" $ f0 $ attack1Class c
  printf "<</T(Attack 1 \\(Enh\\))/V(%s)>>\n" $ f0 $ attack1Enh c
  printf "<</T(Attack 1 \\(Feat\\))/V(%s)>>\n" $ f0 $ attack1Feat c
  printf "<</T(Attack 1 \\(Misc\\))/V(%s)>>\n" $ f0 $ attack1Misc c
  printf "<</T(Attack 1 \\(Prof\\))/V(%d)>>\n" $ attack1Prof c
  printf "<</T(Attack 2 \\(Abil\\))/V(%s)>>\n" $ f0 $ attack2Abil c
  printf "<</T(Attack 2 \\(Ability\\))/V(%s)>>\n" $ attack2Name c
  printf "<</T(Attack 2 \\(Att Bonus\\))/V(%d)>>\n" $ attack2Bonus c
  printf "<</T(Attack 2 \\(Class\\))/V(%s)>>\n" $ f0 $ attack2Class c
  printf "<</T(Attack 2 \\(Enh\\))/V(%s)>>\n" $ f0 $ attack2Enh c
  printf "<</T(Attack 2 \\(Feat\\))/V(%s)>>\n" $ f0 $ attack2Feat c
  printf "<</T(Attack 2 \\(Misc\\))/V(%s)>>\n" $ f0 $ attack2Misc c
  printf "<</T(Attack 2 \\(Prof\\))/V(%d)>>\n" $ attack2Prof c
  printf "<</T(Basic Attack 1 \\(Attack\\))/V(%d)>>\n" $ basic1MeleeAttack c
  printf "<</T(Basic Attack 1 \\(Damage\\))/V(%s)>>\n" $ basic1MeleeDamage c
  printf "<</T(Basic Attack 1 \\(Defense\\))/V(%s)>>\n" $ basic1MeleeDefense c
  printf "<</T(Basic Attack 1 \\(Weapon or Power\\))/V(%s)>>\n" $ basic1MeleeWeaponOrPower c
  printf "<</T(Basic Attack 2 \\(Attack\\))/V(%d)>>\n" $ basic2RangedAttack c
  printf "<</T(Basic Attack 2 \\(Damage\\))/V(%s)>>\n" $ basic2RangedDamage c
  printf "<</T(Basic Attack 2 \\(Defense\\))/V(%s)>>\n" $ basic2RangedDefense c
  printf "<</T(Basic Attack 2 \\(Weapon or Power\\))/V(%s)>>\n" $ basic2RangedWeaponOrPower c
  printf "<</T(Bloodied)/V(%d)>>\n" $ bloodied c
  printf "<</T(Bluff)/V(%d)>>\n" $ bluff c
  printf "<</T(Bluff \\(Abil Mod + 1 / 2 Level\\))/V(%s)>>\n" $ f0 $ bluffAbil c
  printf "<</T(Bluff \\(Misc\\))/V(%s)>>\n" $ f0 $ bluffMisc c
  printf "<</T(Bluff \\(Trained\\))/V/%s>>\n" $ bluffTrained c
  printf "<</T(Cha)/V(%d)>>\n" $ cha c
  printf "<</T(Cha \\(Abil Mod\\))/V(%s)>>\n" $ f0 $ chaAbilMod c
  printf "<</T(Cha \\(Mod + 1 / 2 Level\\))/V(%s)>>\n" $ f0 $ chaAbilLevel c
  printf "<</T(Character Name)/V(%s)>>\n" $ name c
  printf "<</T(Class)/V(%s)>>\n" $ className c
  mapM (\x -> printf "<</T(Class / Path / Destiny Features %d)/V(%s)>>\n" x $ classFeature (x-1) c) [1..14]
  printf "<</RV(<?xml version=\"1.0\"?><body xfa:APIVersion=\"Acroform:2.7.0.0\" xfa:spec=\"2.1\" xmlns=\"http://www.w3.org/1999/xhtml\" xmlns:xfa=\"http://www.xfa.org/schema/xfa-data/1.0/\"><p dir=\"ltr\" style=\"margin-top:0pt;margin-bottom:0pt;font-family:Helvetica;font-size:12pt\"\\>This is a test of putting a lot of text in the coins and other wealth box. Yay, it wraps!</p></body>)/T(Coins and Other Wealth)/V(%s)>>\n" ""
  printf "<</T(Con)/V(%d)>>\n" $ con c
  printf "<</T(Con \\(Abil Mod\\))/V(%d)>>\n" $ conAbilMod c
  printf "<</T(Con \\(Mod + 1 / 2 Level\\))/V(%d)>>\n" $ conAbilLevel c
  mapM (\x -> printf "<</T(Daily Powers %d)/V(%s)>>\n" x $ dailyPower (x-1) c) [1..6]
  printf "<</T(Damage 1)/V(%s)>>\n" $ damage1Desc c
  printf "<</T(Damage 1 \\(Abil\\))/V(%s)>>\n" $ f0 $ damage1Abil c
  printf "<</T(Damage 1 \\(Ability\\))/V(%s)>>\n" $ attack1Name c
  printf "<</T(Damage 1 \\(Enh\\))/V(%s)>>\n" $ f0 $ damage1Enh c
  printf "<</T(Damage 1 \\(Feat\\))/V(%s)>>\n" $ f0 $ damage1Feat c
  printf "<</T(Damage 1 \\(Misc 1\\))/V(%s)>>\n" $ f0 $ damage1Misc1 c
  printf "<</T(Damage 1 \\(Misc 2\\))/V(%s)>>\n" $ f0 $ damage1Misc2 c
  printf "<</T(Damage 2)/V(%s)>>\n" $ damage2Desc c
  printf "<</T(Damage 2 \\(Abil\\))/V(%s)>>\n" $ f0 $ damage2Abil c
  printf "<</T(Damage 2 \\(Ability\\))/V(%s)>>\n" $ attack2Name c
  printf "<</T(Damage 2 \\(Enh\\))/V(%s)>>\n" $ f0 $ damage2Enh c
  printf "<</T(Damage 2 \\(Feat\\))/V(%s)>>\n" $ f0 $ damage2Feat c
  printf "<</T(Damage 2 \\(Misc 1\\))/V(%s)>>\n" $ f0 $ damage2Misc1 c
  printf "<</T(Damage 2 \\(Misc 2\\))/V(%s)>>\n" $ f0 $ damage2Misc2 c
  printf "<</T(Dex)/V(%d)>>\n" $ dex c
  printf "<</T(Dex \\(Abil Mod\\))/V(%d)>>\n" $ dexAbilMod c
  printf "<</T(Dex \\(Mod + 1 / 2 Level\\))/V(%d)>>\n" $ dexAbilLevel c
  printf "<</T(Diplomacy)/V(%d)>>\n" $ diplomacy c
  printf "<</T(Diplomacy \\(Abil Mod + 1 / 2 Level\\))/V(%s)>>\n" $ f0 $ diplomacyAbil c
  printf "<</T(Diplomacy \\(Misc\\))/V(%s)>>\n" $ f0 $ diplomacyMisc c
  printf "<</T(Diplomacy \\(Trained\\))/V/%s>>\n" $ diplomacyTrained c
  printf "<</T(Dungeoneering)/V(%d)>>\n" $ dungeoneering c
  printf "<</T(Dungeoneering \\(Abil Mod + 1 / 2 Level\\))/V(%s)>>\n" $ f0 $ dungeoneeringAbil c
  printf "<</T(Dungeoneering \\(Misc\\))/V(%s)>>\n" $ f0 $ dungeoneeringMisc c
  printf "<</T(Dungeoneering \\(Trained\\))/V/%s>>\n" $ dungeoneeringTrained c
  mapM (\x -> printf "<</T(Encounter Powers %d)/V(%s)>>\n" x $ encounterPower (x-1) c) [1..6]
  printf "<</T(Endurance)/V(%d)>>\n" $ endurance c
  printf "<</T(Endurance \\(Abil Mod + 1 / 2 Level\\))/V(%s)>>\n" $ f0 $ enduranceAbil c
  printf "<</T(Endurance \\(Misc\\))/V(%s)>>\n" $ f0 $ enduranceMisc c
  printf "<</T(Endurance \\(Trained\\))/V/%s>>\n" $ enduranceTrained c
  -- Do a similar iteration for skills, and defenses
  mapM (\x -> printf "<</T(Feats %d)/V(%s)>>\n" x $ feat (x-1) c) [1..17]
  printf "<</T(Fort)/V(%d)>>\n" $ fort c
  printf "<</T(Fort \\(Abil\\))/V(%s)>>\n" $ f0 $ fortAbil c
  printf "<</T(Fort \\(Class\\))/V(%s)>>\n" $ f0 $ fortClass c
  printf "<</T(Fort \\(Enh\\))/V(%s)>>\n" $ f0 $ fortEnh c
  printf "<</T(Fort \\(Feat\\))/V(%s)>>\n" $ f0 $ fortFeat c
  printf "<</T(Fort \\(Misc 1\\))/V(%s)>>\n" $ f0 $ fortMisc1 c
  printf "<</T(Fort \\(Misc 2\\))/V(%s)>>\n" $ f0 $ fortMisc2 c
  printf "<</T(Gender)/V(%s)>>\n" $ gender c
  printf "<</T(Heal)/V(%d)>>\n" $ heal c
  printf "<</T(Heal \\(Abil Mod + 1 / 2 Level\\))/V(%s)>>\n" $ f0 $ healAbil c
  printf "<</T(Heal \\(Misc\\))/V(%s)>>\n" $ f0 $ healMisc c
  printf "<</T(Heal \\(Trained\\))/V/%s>>\n" $ healTrained c
  printf "<</T(Height)/V(%s)>>\n" "5' 9\""
  printf "<</T(History)/V(%d)>>\n" $ history c
  printf "<</T(History \\(Abil Mod + 1 / 2 Level\\))/V(%s)>>\n" $ f0 $ historyAbil c
  printf "<</T(History \\(Misc\\))/V(%s)>>\n" $ f0 $ historyMisc c
  printf "<</T(History \\(Trained\\))/V/%s>>\n" $ historyTrained c
  printf "<</T(Initiative)/V(%d)>>\n" $ C.init c
  printf "<</T(Initiative \\(Dex\\))/V(%d)>>\n" $ dexAbilMod c
  printf "<</T(Initiative \\(Misc\\))/V(%s)>>\n" $ f0 $ initMisc c
  printf "<</T(Insight)/V(%d)>>\n" $ insight c
  printf "<</T(Insight \\(Abil Mod + 1 / 2 Level\\))/V(%s)>>\n" $ f0 $ insightAbil c
  printf "<</T(Insight \\(Misc\\))/V(%s)>>\n" $ f0 $ insightMisc c
  printf "<</T(Insight \\(Trained\\))/V/%s>>\n" $ insightTrained c
  printf "<</T(Int)/V(%d)>>\n" $ int c
  printf "<</T(Int \\(Abil Mod\\))/V(%d)>>\n" $ intAbilMod c
  printf "<</T(Int \\(Mod + 1 / 2 Level\\))/V(%d)>>\n" $ intAbilLevel c
  printf "<</T(Intimidate)/V(%d)>>\n" $ intimidate c
  printf "<</T(Intimidate \\(Abil Mod + 1 / 2 Level\\))/V(%s)>>\n" $ f0 $ intimidateAbil c
  printf "<</T(Intimidate \\(Misc\\))/V(%s)>>\n" $ f0 $ intimidateMisc c
  printf "<</T(Intimidate \\(Trained\\))/V/%s>>\n" $ intimidateTrained c
  mapM (\x -> printf "<</T(Languages Known %d)/V(%s)>>\n" x $ language (x-1) c) [1..3]
  printf "<</T(Level)/V(%d)>>\n" $ level c
  mapM (\x -> printf "<</T(Magic Items \\(Weapon %d\\))/V(%s)>>\n" (x :: Int) $ magicItemInSlot (x-1) "Weapon" c) [1..4]
  printf "<</T(Magic Items \\(Armor\\))/V(%s)>>\n" $ magicItemInSlot 0 "Armor" c
  printf "<</T(Magic Items \\(Arms\\))/V(%s)>>\n" $ magicItemInSlot 0 "Arms" c
  printf "<</T(Magic Items \\(Feet\\))/V(%s)>>\n" $ magicItemInSlot 0 "Feet" c
  printf "<</T(Magic Items \\(Hands\\))/V(%s)>>\n" $ magicItemInSlot 0 "Hands" c
  printf "<</T(Magic Items \\(Head\\))/V(%s)>>\n" $ magicItemInSlot 0 "Head" c
  printf "<</T(Magic Items \\(Neck\\))/V(%s)>>\n" $ magicItemInSlot 0 "Neck" c
  printf "<</T(Magic Items \\(Ring 1\\))/V(%s)>>\n" $ magicItemInSlot 0 "Ring" c
  printf "<</T(Magic Items \\(Ring 2\\))/V(%s)>>\n" $ magicItemInSlot 1 "Ring" c
  printf "<</T(Magic Items \\(Waist\\))/V(%s)>>\n" $ magicItemInSlot 0 "Waist" c
  mapM (\x -> printf "<</T(Magic Items %d)/V(%s)>>\n" x $ magicItem (x-1) c) [1..8]
  printf "<</T(Max HP)/V(%d)>>\n" $ hp c
  printf "<</T(Nature)/V(%d)>>\n" $ nature c
  printf "<</T(Nature \\(Abil Mod + 1 / 2 Level\\))/V(%s)>>\n" $ f0 $ natureAbil c
  printf "<</T(Nature \\(Misc\\))/V(%s)>>\n" $ f0 $ natureMisc c
  printf "<</T(Nature \\(Trained\\))/V/%s>>\n" $ natureTrained c
  mapM (\x -> printf "<</T(Other Equipment %d)/V(%s)>>\n" x $ otherEquipment (x-1) c) [1..10]
  printf "<</T(Passive Insight)/V(%d)>>\n" $ 10 + insight c
  printf "<</T(Passive Perception)/V(%d)>>\n" $ 10 + perception c
  printf "<</T(Perception)/V(%d)>>\n" $ perception c
  printf "<</T(Perception \\(Abil Mod + 1 / 2 Level\\))/V(%s)>>\n" $ f0 $ perceptionAbil c
  printf "<</T(Perception \\(Misc\\))/V(%s)>>\n" $ f0 $ perceptionMisc c
  printf "<</T(Perception \\(Trained\\))/V/%s>>\n" $ perceptionTrained c
  printf "<</T(Player Name)/V(%s)>>\n" $ playerName c
  printf "<</T(Race)/V(%s)>>\n" $ raceName c
  printf "<</T(Race Features \\(Ability Score Mods\\))/V(%s)>>\n" $ racialAbilModifiers c
  mapM (\x -> printf "<</T(Race Features %d)/V(%s)>>\n" x $ racialFeature (x-1) c) [1..8]
  printf "<</T(Ref)/V(%d)>>\n" $ ref c
  printf "<</T(Ref \\(Abil\\))/V(%s)>>\n" $ f0 $ refAbil c
  printf "<</T(Ref \\(Class\\))/V(%s)>>\n" $ f0 $ refClass c
  printf "<</T(Ref \\(Enh\\))/V(%s)>>\n" $ f0 $ refEnh c
  printf "<</T(Ref \\(Feat\\))/V(%s)>>\n" $ f0 $ refFeat c
  printf "<</T(Ref \\(Misc 1\\))/V(%s)>>\n" $ f0 $ refMisc1 c
  printf "<</T(Ref \\(Misc 2\\))/V(%s)>>\n" $ f0 $ refMisc2 c
  printf "<</T(Religion)/V(%d)>>\n" $ religion c
  printf "<</T(Religion \\(Abil Mod + 1 / 2 Level\\))/V(%s)>>\n" $ f0 $ religionAbil c
  printf "<</T(Religion \\(Misc\\))/V(%s)>>\n" $ f0 $ religionMisc c
  printf "<</T(Religion \\(Trained\\))/V/%s>>\n" $ religionTrained c
  printf "<</T(Saving Throw Mods)/V(%s)>>\n" $ savingThrowMods c
  printf "<</T(Size)/V(%s)>>\n" $ size c
  printf "<</T(Skill Armor Penalty)/V(%s)>>\n" $ f0 $ skillArmorPenalty c
  printf "<</T(Speed)/V(%d)>>\n" $ speed c
  printf "<</T(Speed \\(Armor\\))/V(%s)>>\n" $ f0 $ speedArmor c
  printf "<</T(Speed \\(Base\\))/V(%d)>>\n" $ speedBase c
  printf "<</T(Stealth)/V(%d)>>\n" $ stealth c
  printf "<</T(Stealth \\(Abil Mod + 1 / 2 Level\\))/V(%s)>>\n" $ f0 $ stealthAbil c
  printf "<</T(Stealth \\(Misc\\))/V(%s)>>\n" $ f0 $ stealthMisc c
  printf "<</T(Stealth \\(Trained\\))/V/%s>>\n" $ stealthTrained c
  printf "<</T(Str)/V(%d)>>\n" $ str c
  printf "<</T(Str \\(Abil Mod\\))/V(%d)>>\n" $ strAbilMod c
  printf "<</T(Str \\(Mod + 1 / 2 Level\\))/V(%d)>>\n" $ strAbilLevel c
  printf "<</T(Streetwise)/V(%d)>>\n" $ streetwise c
  printf "<</T(Streetwise \\(Abil Mod + 1 / 2 Level\\))/V(%s)>>\n" $ f0 $ streetwiseAbil c
  printf "<</T(Streetwise \\(Misc\\))/V(%s)>>\n" $ f0 $ streetwiseMisc c
  printf "<</T(Streetwise \\(Trained\\))/V/%s>>\n" $ streetwiseTrained c
  printf "<</T(Surge Value)/V(%d)>>\n" $ surgeValue c
  printf "<</T(Surges / Day)/V(%d)>>\n" $ surgesDay c
  printf "<</T(Thievery)/V(%d)>>\n" $ thievery c
  printf "<</T(Thievery \\(Abil Mod + 1 / 2 Level\\))/V(%s)>>\n" $ f0 $ thieveryAbil c
  printf "<</T(Thievery \\(Misc\\))/V(%s)>>\n" $ f0 $ thieveryMisc c
  printf "<</T(Thievery \\(Trained\\))/V/%s>>\n" $ thieveryTrained c
  printf "<</T(Total XP)/V(%d)>>\n" $ xp c
  mapM (\x -> printf "<</T(Utility Powers %d)/V(%s)>>\n" x $ utilityPower (x-1) c) [1..8]
  printf "<</T(Weight)/V(%s)>>\n" $ weight c
  printf "<</T(Will)/V(%d)>>\n" $ will c
  printf "<</T(Will \\(Abil\\))/V(%s)>>\n" $ f0 $ willAbil c
  printf "<</T(Will \\(Class\\))/V(%s)>>\n" $ f0 $ willClass c
  printf "<</T(Will \\(Enh\\))/V(%s)>>\n" $ f0 $ willEnh c
  printf "<</T(Will \\(Feat\\))/V(%s)>>\n" $ f0 $ willFeat c
  printf "<</T(Will \\(Misc 1\\))/V(%s)>>\n" $ f0 $ willMisc1 c
  printf "<</T(Will \\(Misc 2\\))/V(%s)>>\n" $ f0 $ willMisc2 c
  printf "<</T(Wis)/V(%d)>>\n" $ wis c
  printf "<</T(Wis \\(Abil Mod\\))/V(%d)>>\n" $ wisAbilMod c
  printf "<</T(Wis \\(Mod + 1 / 2 Level\\))/V(%d)>>\n" $ wisAbilLevel c


main = do
  printFdfHeader
  printFdfData pontus
  printFdfFooter