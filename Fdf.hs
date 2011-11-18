module Fdf where

import Text.Printf
import Skill
import Character as C
import Pontus

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
  printf "<</T(1 / 2 Level)/V(%d)>>" $ halfLevel c
  printf "<</T(10 + 1 / 2 Level)/V(%d)>>" $ tenPlusHalfLevel c
  printf "<</T(AC)/V(%d)>>" $ ac c
  printf "<</T(AC \\(Armor / Ability\\))/V(%d)>>" $ acArmorAbility c
  printf "<</T(AC \\(Class\\))/V(%d)>>" $ acClass c
  printf "<</T(AC \\(Enhancement\\))/V(%d)>>" $ acEnh c
  printf "<</T(AC \\(Feat\\))/V(%d)>>" $ acFeat c
  printf "<</T(AC \\(Misc 1\\))/V(%d)>>" $ acMisc1 c
  printf "<</T(AC \\(Misc 2\\))/V(%d)>>" $ acMisc2 c
  printf "<</T(Acrobatics)/V(%d)>>" $ acrobatics c
  printf "<</T(Acrobatics \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" $ acrobaticsAbil c
  printf "<</T(Arcobatics \\(Misc\\))/V(%d)>>" $ acrobaticsMisc c -- field does not exist?? Typo! Arcobatics
  printf "<</T(Acrobatics \\(Trained\\))/V/%s>>" $ acrobaticsTrained c
  printf "<</T(Age)/V(%d)>>" $ age c
  printf "<</T(Alignment)/V(%s)>>" $ alignment c
  printf "<</T(Arcana)/V(%d)>>" $ arcana c
  printf "<</T(Arcana \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" $ arcanaAbil c
  printf "<</T(Arcana \\(Misc\\))/V(%d)>>" $ arcanaMisc c
  printf "<</T(Arcana \\(Trained\\))/V/%s>>" $ arcanaTrained c
  printf "<</T(Athletics)/V(%d)>>" $ arcana c
  printf "<</T(Athletics \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" $ athleticsAbil c
  printf "<</T(Athletics \\(Misc\\))/V(%d)>>" $ athleticsMisc c
  printf "<</T(Athletics \\(Trained\\))/V/%s>>" $ athleticsTrained c
  printf "<</T(Attack 1 \\(Abil\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Attack 1 \\(Ability\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Attack 1 \\(Att Bonus\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Attack 1 \\(Enh\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Attack 1 \\(Prof\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Attack 2 \\(Abil\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Attack 2 \\(Ability\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Attack 2 \\(Att Bonus\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Attack 2 \\(Enh\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Attack 2 \\(Prof\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Basic Attack 1 \\(Attack\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Basic Attack 1 \\(Damage\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Basic Attack 1 \\(Defense\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Basic Attack 1 \\(Weapon or Power\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Basic Attack 2 \\(Attack\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Basic Attack 2 \\(Damage\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Basic Attack 2 \\(Defense\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Basic Attack 2 \\(Weapon or Power\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Bloodied)/V(%d)>>" $ bloodied c
  printf "<</T(Bluff)/V(%d)>>" $ bluff c
  printf "<</T(Bluff \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" $ bluffAbil c
  printf "<</T(Bluff \\(Misc\\))/V(%d)>>" $ bluffMisc c
  printf "<</T(Bluff \\(Trained\\))/V/%s>>" $ bluffTrained c
  printf "<</T(Cha)/V(%d)>>" $ cha c
  printf "<</T(Cha \\(Abil Mod\\))/V(%d)>>" $ chaAbilMod c
  printf "<</T(Cha \\(Mod + 1 / 2 Level\\))/V(%d)>>" $ chaAbilLevel c
  printf "<</T(Character Name)/V(%s)>>" $ name c
  printf "<</T(Class)/V(%s)>>" $ className c
  printf "<</T(Class / Path / Destiny Features 1)/V(%d)>>" (0 :: Int)
  printf "<</RV(<?xml version=\"1.0\"?><body xfa:APIVersion=\"Acroform:2.7.0.0\" xfa:spec=\"2.1\" xmlns=\"http://www.w3.org/1999/xhtml\" xmlns:xfa=\"http://www.xfa.org/schema/xfa-data/1.0/\"><p dir=\"ltr\" style=\"margin-top:0pt;margin-bottom:0pt;font-family:Helvetica;font-size:12pt\"\\>This is a test of putting a lot of text in the coins and other wealth box. Yay, it wraps!</p></body>)/T(Coins and Other Wealth)/V(%s)>>" ""
  printf "<</T(Con)/V(%d)>>" $ con c
  printf "<</T(Con \\(Abil Mod\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Con \\(Mod + 1 / 2 Level\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Damage 1)/V(%d)>>" (0 :: Int)
  printf "<</T(Damage 1 \\(Abil\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Damage 1 \\(Ability\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Damage 1 \\(Enh\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Damage 2)/V(%d)>>" (0 :: Int)
  printf "<</T(Damage 2 \\(Abil\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Damage 2 \\(Ability\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Dex)/V(%d)>>" $ dex c
  printf "<</T(Dex \\(Abil Mod\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Dex \\(Mod + 1 / 2 Level\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Diplomacy)/V(%d)>>" (0 :: Int)
  printf "<</T(Diplomacy \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" $ diplomacyAbil c
  printf "<</T(Diplomacy \\(Misc\\))/V(%d)>>" $ diplomacyMisc c
  printf "<</T(Diplomacy \\(Trained\\))/V/%s>>" $ diplomacyTrained c
  printf "<</T(Dungeoneering)/V(%d)>>" $ dungeoneering c
  printf "<</T(Dungeoneering \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" $ dungeoneeringAbil c
  printf "<</T(Dungeoneering \\(Misc\\))/V(%d)>>" $ dungeoneeringMisc c
  printf "<</T(Dungeoneering \\(Trained\\))/V/%s>>" $ dungeoneeringTrained c
  printf "<</T(Endurance)/V(%d)>>" (0 :: Int)
  printf "<</T(Endurance \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" $ enduranceAbil c
  printf "<</T(Endurance \\(Misc\\))/V(%d)>>" $ enduranceMisc c
  printf "<</T(Endurance \\(Trained\\))/V/%s>>" $ enduranceTrained c
  -- Do a similar iteration for skills, and defenses
  mapM (\x -> printf "<</T(Feats %d)/V(%s)>>" x $ feat (x-1) c) [1..17]
  printf "<</T(Fort)/V(%d)>>" $ fort c
  printf "<</T(Fort \\(Abil\\))/V(%d)>>" $ fortAbil c
  printf "<</T(Fort \\(Class\\))/V(%d)>>" $ fortClass c
  printf "<</T(Fort \\(Enh\\))/V(%d)>>" $ fortEnh c
  printf "<</T(Fort \\(Feat\\))/V(%d)>>" $ fortFeat c
  printf "<</T(Fort \\(Misc 1\\))/V(%d)>>" $ fortMisc1 c
  printf "<</T(Fort \\(Misc 2\\))/V(%d)>>" $ fortMisc2 c
  printf "<</T(Gender)/V(%d)>>" (0 :: Int)
  printf "<</T(Heal)/V(%d)>>" $ heal c
  printf "<</T(Heal \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" $ healAbil c
  printf "<</T(Heal \\(Misc\\))/V(%d)>>" $ healMisc c
  printf "<</T(Heal \\(Trained\\))/V/%s>>" $ healTrained c
  printf "<</T(Height)/V(%s)>>" "5' 9\""
  printf "<</T(History)/V(%d)>>" (0 :: Int)
  printf "<</T(History \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" $ historyAbil c
  printf "<</T(History \\(Misc\\))/V(%d)>>" $ historyMisc c
  printf "<</T(History \\(Trained\\))/V/%s>>" $ historyTrained c
  printf "<</T(Initiative)/V(%d)>>" (0 :: Int)
  printf "<</T(Initiative \\(Dex\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Insight)/V(%d)>>" $ insight c
  printf "<</T(Insight \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" $ insightAbil c
  printf "<</T(Insight \\(Misc\\))/V(%d)>>" $ insightMisc c
  printf "<</T(Insight \\(Trained\\))/V/%s>>" $ insightTrained c
  printf "<</T(Int)/V(%d)>>" $ int c
  printf "<</T(Int \\(Abil Mod\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Int \\(Mod + 1 / 2 Level\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Intimidate)/V(%d)>>" (0 :: Int)
  printf "<</T(Intimidate \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" $ intimidateAbil c
  printf "<</T(Intimidate \\(Misc\\))/V(%d)>>" $ intimidateMisc c
  printf "<</T(Intimidate \\(Trained\\))/V/%s>>" $ intimidateTrained c
  printf "<</T(Languages Known 1)/V(%d)>>" (0 :: Int)
  printf "<</T(Level)/V(%d)>>" $ level c
  printf "<</T(Max HP)/V(%d)>>" $ hp c
  printf "<</T(Nature)/V(%d)>>" $ nature c
  printf "<</T(Nature \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" $ natureAbil c
  printf "<</T(Nature \\(Misc\\))/V(%d)>>" $ natureMisc c
  printf "<</T(Nature \\(Trained\\))/V/%s>>" $ natureTrained c
  printf "<</T(Passive Insight)/V(%d)>>" $ 10 + insight c
  printf "<</T(Passive Perception)/V(%d)>>" $ 10 + perception c
  printf "<</T(Perception)/V(%d)>>" $ perception c
  printf "<</T(Perception \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" $ perceptionAbil c
  printf "<</T(Perception \\(Misc\\))/V(%d)>>" $ perceptionMisc c
  printf "<</T(Perception \\(Trained\\))/V/%s>>" $ perceptionTrained c
  printf "<</T(Player Name)/V(%d)>>" (0 :: Int)
  printf "<</T(Race)/V(%s)>>" $ raceName c
  printf "<</T(Race Features \\(Ability Score Mods\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Race Features 1)/V(%d)>>" (0 :: Int)
  printf "<</T(Race Features 2)/V(%d)>>" (0 :: Int)
  printf "<</T(Ref)/V(%d)>>" $ ref c
  printf "<</T(Ref \\(Abil\\))/V(%d)>>" $ refAbil c
  printf "<</T(Ref \\(Class\\))/V(%d)>>" $ refClass c
  printf "<</T(Ref \\(Enh\\))/V(%d)>>" $ refEnh c
  printf "<</T(Ref \\(Feat\\))/V(%d)>>" $ refFeat c
  printf "<</T(Ref \\(Misc 1\\))/V(%d)>>" $ refMisc1 c
  printf "<</T(Ref \\(Misc 2\\))/V(%d)>>" $ refMisc2 c
  printf "<</T(Religion)/V(%d)>>" $ religion c
  printf "<</T(Religion \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" $ religionAbil c
  printf "<</T(Religion \\(Misc\\))/V(%d)>>" $ religionMisc c
  printf "<</T(Religion \\(Trained\\))/V/%s>>" $ religionTrained c
  printf "<</T(Saving Throw Mods)/V(%d)>>" (0 :: Int)
  printf "<</T(Size)/V(%s)>>" $ size c
  printf "<</T(Skill Armor Penalty)/V(%d)>>" $ skillArmorPenalty c
  printf "<</T(Speed)/V(%d)>>" $ speed c
  printf "<</T(Speed \\(Armor\\))/V(%d)>>" $ speedArmor c
  printf "<</T(Speed \\(Base\\))/V(%d)>>" $ speedBase c
  printf "<</T(Stealth)/V(%d)>>" $ stealth c
  printf "<</T(Stealth \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" $ stealthAbil c
  printf "<</T(Stealth \\(Misc\\))/V(%d)>>" $ stealthMisc c
  printf "<</T(Stealth \\(Trained\\))/V/%s>>" $ stealthTrained c
  printf "<</T(Str)/V(%d)>>" $ str c
  printf "<</T(Str \\(Abil Mod\\))/V(%d)>>" $ strAbilMod c
  printf "<</T(Str \\(Mod + 1 / 2 Level\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Streetwise)/V(%d)>>" $ streetwise c
  printf "<</T(Streetwise \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" $ streetwiseAbil c
  printf "<</T(Streetwise \\(Misc\\))/V(%d)>>" $ streetwiseMisc c
  printf "<</T(Streetwise \\(Trained\\))/V/%s>>" $ streetwiseTrained c
  printf "<</T(Surge Value)/V(%d)>>" $ surgeValue c
  printf "<</T(Surges / Day)/V(%d)>>" $ surgesDay c
  printf "<</T(Thievery)/V(%d)>>" $ thievery c
  printf "<</T(Thievery \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" $ thieveryAbil c
  printf "<</T(Thievery \\(Misc\\))/V(%d)>>" $ thieveryMisc c
  printf "<</T(Thievery \\(Trained\\))/V/%s>>" $ thieveryTrained c
  printf "<</T(Total XP)/V(%d)>>" $ xp c
  printf "<</T(Weight)/V(%s)>>" $ weight c
  printf "<</T(Will)/V(%d)>>" $ will c
  printf "<</T(Will \\(Abil\\))/V(%d)>>" $ willAbil c
  printf "<</T(Will \\(Class\\))/V(%d)>>" $ willClass c
  printf "<</T(Will \\(Enh\\))/V(%d)>>" $ willEnh c
  printf "<</T(Will \\(Feat\\))/V(%d)>>" $ willFeat c
  printf "<</T(Will \\(Misc 1\\))/V(%d)>>" $ willMisc1 c
  printf "<</T(Will \\(Misc 2\\))/V(%d)>>" $ willMisc2 c
  printf "<</T(Wis)/V(%d)>>" $ wis c
  printf "<</T(Wis \\(Abil Mod\\))/V(%d)>>" $ wisAbilMod c
  printf "<</T(Wis \\(Mod + 1 / 2 Level\\))/V(%d)>>" (0 :: Int)


main = do
  printFdfHeader
  printFdfData pontus
  printFdfFooter