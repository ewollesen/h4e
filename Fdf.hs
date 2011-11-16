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
  printf "<</T(Acrobatics \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" $ (0 :: Int)
  printf "<</T(Acrobatics \\(Trained\\))/V/%s>>" $ acrobaticsTrained c
  printf "<</T(Age)/V(%d)>>" (0 :: Int)
  printf "<</T(Alignment)/V(%d)>>" (0 :: Int)
  printf "<</T(Arcana)/V(%d)>>" (0 :: Int)
  printf "<</T(Arcana \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Arcana \\(Trained\\))/V/%s>>" "Yes"
  printf "<</T(Athletics)/V(%d)>>" (0 :: Int)
  printf "<</T(Athletics \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Athletics \\(Trained\\))/V/%s>>" "Yes"
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
  printf "<</T(Bloodied)/V(%d)>>" (0 :: Int)
  printf "<</T(Bluff)/V(%d)>>" (0 :: Int)
  printf "<</T(Bluff \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Bluff \\(Trained\\))/V/%s>>" "Yes"
  printf "<</T(Cha)/V(%d)>>" (0 :: Int)
  printf "<</T(Cha \\(Abil Mod\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Cha \\(Mod + 1 / 2 Level\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Character Name)/V(%d)>>" (0 :: Int)
  printf "<</T(Class)/V(%d)>>" (0 :: Int)
  printf "<</T(Class / Path / Destiny Features 1)/V(%d)>>" (0 :: Int)
  printf "<</RV(<?xml version=\"1.0\"?><body xfa:APIVersion=\"Acroform:2.7.0.0\" xfa:spec=\"2.1\" xmlns=\"http://www.w3.org/1999/xhtml\" xmlns:xfa=\"http://www.xfa.org/schema/xfa-data/1.0/\"><p dir=\"ltr\" style=\"margin-top:0pt;margin-bottom:0pt;font-family:Helvetica;font-size:12pt\"\\>This is a test of putting a lot of text in the coins and other wealth box. Yay, it wraps!</p></body>)/T(Coins and Other Wealth)/V(%s)>>" ""
  printf "<</T(Con)/V(%d)>>" (0 :: Int)
  printf "<</T(Con \\(Abil Mod\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Con \\(Mod + 1 / 2 Level\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Damage 1)/V(%d)>>" (0 :: Int)
  printf "<</T(Damage 1 \\(Abil\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Damage 1 \\(Ability\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Damage 1 \\(Enh\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Damage 2)/V(%d)>>" (0 :: Int)
  printf "<</T(Damage 2 \\(Abil\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Damage 2 \\(Ability\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Dex)/V(%d)>>" (0 :: Int)
  printf "<</T(Dex \\(Abil Mod\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Dex \\(Mod + 1 / 2 Level\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Diplomacy)/V(%d)>>" (0 :: Int)
  printf "<</T(Diplomacy \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Diplomacy \\(Trained\\))/V/%s>>" "Yes"
  printf "<</T(Dungeoneering)/V(%d)>>" (0 :: Int)
  printf "<</T(Dungeoneering \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Dungeoneering \\(Trained\\))/V/%s>>" "Yes"
  printf "<</T(Endurance)/V(%d)>>" (0 :: Int)
  printf "<</T(Endurance \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Endurance \\(Misc\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Endurance \\(Trained\\))/V/%s>>" "Yes"
  printf "<</T(Feats 1)/V(%d)>>" (0 :: Int)
  printf "<</T(Fort)/V(%d)>>" (0 :: Int)
  printf "<</T(Fort \\(Abil\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Fort \\(Misc 1\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Gender)/V(%d)>>" (0 :: Int)
  printf "<</T(Heal)/V(%d)>>" $ heal c
  printf "<</T(Heal \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Heal \\(Trained\\))/V/%s>>" $ healTrained c
  printf "<</T(Height)/V(%s)>>" "5' 9\""
  printf "<</T(History)/V(%d)>>" (0 :: Int)
  printf "<</T(History \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" (0 :: Int)
  printf "<</T(History \\(Trained\\))/V/%s>>" "Yes"
  printf "<</T(Initiative)/V(%d)>>" (0 :: Int)
  printf "<</T(Initiative \\(Dex\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Insight)/V(%d)>>" (0 :: Int)
  printf "<</T(Insight \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Insight \\(Trained\\))/V/%s>>" "Yes"
  printf "<</T(Int)/V(%d)>>" (0 :: Int)
  printf "<</T(Int \\(Abil Mod\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Int \\(Mod + 1 / 2 Level\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Intimidate)/V(%d)>>" (0 :: Int)
  printf "<</T(Intimidate \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Intimidate \\(Trained\\))/V/%s>>" "Yes"
  printf "<</T(Languages Known 1)/V(%d)>>" (0 :: Int)
  printf "<</T(Level)/V(%d)>>" (0 :: Int)
  printf "<</T(Max HP)/V(%d)>>" (0 :: Int)
  printf "<</T(Nature)/V(%d)>>" (0 :: Int)
  printf "<</T(Nature \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Nature \\(Trained\\))/V/%s>>" "Yes"
  printf "<</T(Passive Insight)/V(%d)>>" (0 :: Int)
  printf "<</T(Passive Perception)/V(%d)>>" (0 :: Int)
  printf "<</T(Perception)/V(%d)>>" (0 :: Int)
  printf "<</T(Perception \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Perception \\(Trained\\))/V/%s>>" "Yes"
  printf "<</T(Player Name)/V(%d)>>" (0 :: Int)
  printf "<</T(Race)/V(%d)>>" (0 :: Int)
  printf "<</T(Race Features \\(Ability Score Mods\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Race Features 1)/V(%d)>>" (0 :: Int)
  printf "<</T(Race Features 2)/V(%d)>>" (0 :: Int)
  printf "<</T(Ref)/V(%d)>>" (0 :: Int)
  printf "<</T(Ref \\(Abil\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Ref \\(Misc 1\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Ref \\(Misc 2\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Religion)/V(%d)>>" (0 :: Int)
  printf "<</T(Religion \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Religion \\(Trained\\))/V/%s>>" "Yes"
  printf "<</T(Saving Throw Mods)/V(%d)>>" (0 :: Int)
  printf "<</T(Size)/V(%d)>>" (0 :: Int)
  printf "<</T(Skill Armor Penalty)/V(%d)>>" (0 :: Int)
  printf "<</T(Speed)/V(%d)>>" (0 :: Int)
  printf "<</T(Speed \\(Armor\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Speed \\(Base\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Stealth)/V(%d)>>" (0 :: Int)
  printf "<</T(Stealth \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Stealth \\(Trained\\))/V/%s>>" "Yes"
  printf "<</T(Str)/V(%d)>>" (0 :: Int)
  printf "<</T(Str \\(Abil Mod\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Str \\(Mod + 1 / 2 Level\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Streetwise)/V(%d)>>" (0 :: Int)
  printf "<</T(Streetwise \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Streetwise \\(Trained\\))/V/%s>>" "Yes"
  printf "<</T(Surge Value)/V(%d)>>" (0 :: Int)
  printf "<</T(Surges / Day)/V(%d)>>" (0 :: Int)
  printf "<</T(Thievery)/V(%d)>>" (0 :: Int)
  printf "<</T(Thievery \\(Abil Mod + 1 / 2 Level\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Thievery \\(Trained\\))/V/%s>>" "Yes"
  printf "<</T(Total XP)/V(%d)>>" (0 :: Int)
  printf "<</T(Weight)/V(%d)>>" (0 :: Int)
  printf "<</T(Will)/V(%d)>>" (0 :: Int)
  printf "<</T(Will \\(Abil\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Will \\(Class\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Will \\(Misc 1\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Wis)/V(%d)>>" (0 :: Int)
  printf "<</T(Wis \\(Abil Mod\\))/V(%d)>>" (0 :: Int)
  printf "<</T(Wis \\(Mod + 1 / 2 Level\\))/V(%d)>>" (0 :: Int)


main = do
  printFdfHeader
  printFdfData pontus
  printFdfFooter