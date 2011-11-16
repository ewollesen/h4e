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
  printf "<< /T (Player name) /V (%s) >>\n" $ playerName c
  printf "<< /T (Character name) /V (%s) >>\n" $ name c
  printf "<< /T (Level) /V (%d) >>\n" $ level c
  printf "<< /T (Class) /V (%s) >>\n" $ className c
  printf "<< /T (Paragon path) /V (%s) >>\n" "" -- TODO
  printf "<< /T (Epic destiny) /V (%s) >>\n" "" -- TODO
  printf "<< /T (Total XP) /V (%d) >>\n" $ xp c
  printf "<< /T (Race) /V (%s) >>\n" $ raceName c
  printf "<< /T (Size) /V (%s) >>\n" $ size c
  printf "<< /T (Age) /V (%d) >>\n" $ age c
  printf "<< /T (Gender) /V (%s) >>\n" $ gender c
  printf "<< /T (Height) /V (%s) >>\n" $ height c
  printf "<< /T (Weight) /V (%s) >>\n" $ weight c
  printf "<< /T (Alignment) /V (%s) >>\n" $ alignment c
  printf "<< /T (Deity) /V (%s) >>\n" $ deity c
  printf "<< /T (Adventuring company or other affiliation) /V (%s) >>\n" $ adventuringCompanyOrOtherAffiliations c
  printf "<< /T (Dex mod to Init) /V (%d) >>\n" $ dexMod c
  printf "<< /T (Half-level mod to Init) /V (%d) >>\n" $ halfLevel c
  printf "<< /T (Misc. mod to Init) /V (%d) >>\n" $ miscModToInit c
  printf "<< /T (Init) /V (%d) >>\n" $ initiative c
  printf "<< /T (Conditional Init mods) /V (%s) >>\n" "" -- TODO
  printf "<< /T (Ten plus half-level mod to AC) /V (%d) >>\n" $ tenPlusHalfLevel c
  printf "<< /T (Armor or ability mod to AC) /V (%d) >>\n" $ armorAndAbilityModToAC c -- TODO rename field
  printf "<< /T (Class mod to AC) /V (%d) >>\n" $ classModToAC c
  printf "<< /T (Feat mod to AC) /V (%d) >>\n" $ featModToAC c
  printf "<< /T (Enhancement mod to AC) /V (%d) >>\n" $ enhModToAC c
  printf "<< /T (Misc. mod 1 to AC) /V (%d) >>\n" $ misc1ModToAC c
  printf "<< /T (Misc. mod 2 to AC) /V (%d) >>\n" $ misc2ModToAC c
  printf "<< /T (AC) /V (%d) >>\n" $ ac c
  printf "<< /T (Ten plus half-level to Fortitude) /V (%d) >>\n" $ tenPlusHalfLevel c
  printf "<< /T (Ability mod to Fortitude) /V (%d) >>\n" $ abilModToFort c
  printf "<< /T (Class mod to Fortitude) /V (%d) >>\n" $ classModToFort c
  printf "<< /T (Feat mod to Fortitude) /V (%d) >>\n" $ featModToFort c
  printf "<< /T (Enhancement mod to Fortitude) /V (%d) >>\n" $ enhModToFort c
  printf "<< /T (Misc. mod 1 to Fortitude) /V (%d) >>\n" $ misc1ModToFort c
  printf "<< /T (Misc. mod 2 to Fortitude) /V (%d) >>\n" $ misc2ModToFort c
  printf "<< /T (Fortitude) /V (%d) >>\n" $ fortitude c
  printf "<< /T (Ten plus half-level to Reflex) /V (%d) >>\n" $ tenPlusHalfLevel c
  printf "<< /T (Ability mod to Reflex) /V (%d) >>\n" $ abilModToRef c
  printf "<< /T (Class mod to Reflex) /V (%d) >>\n" $ classModToRef c
  printf "<< /T (Feat mod to Reflex) /V (%d) >>\n" $ featModToRef c
  printf "<< /T (Enhancement mod to Reflex) /V (%d) >>\n" $ enhModToRef c
  printf "<< /T (Misc. mod 1 to Reflex) /V (%d) >>\n" $ misc1ModToRef c
  printf "<< /T (Misc. mod 2 to Reflex) /V (%d) >>\n" $ misc2ModToRef c
  printf "<< /T (Reflex) /V (%d) >>\n" $ reflex c
  printf "<< /T (Ten plus half-level to Will) /V (%d) >>\n" $ tenPlusHalfLevel c
  printf "<< /T (Ability mod to Will) /V (%d) >>\n" $ abilModToWill c
  printf "<< /T (Class mod to Will) /V (%d) >>\n" $ classModToWill c
  printf "<< /T (Feat mod to Will) /V (%d) >>\n" $ featModToWill c
  printf "<< /T (Enhancement mod to Will) /V (%d) >>\n" $ enhModToWill c
  printf "<< /T (Misc. mod 1 to Will) /V (%d) >>\n" $ misc1ModToWill c
  printf "<< /T (Misc. mod 2 to Will) /V (%d) >>\n" $ misc2ModToWill c
  printf "<< /T (Will) /V (%d) >>\n" $ will c
  printf "<< /T (Additional effects to spending action points) /V (%s) >>\n" "" -- TODO
  printf "<< /T (Conditional mods to AC) /V (%s) >>\n" "" -- TODO
  printf "<< /T (Conditional mods to Fortitude) /V (%s) >>\n" "" -- TODO
  printf "<< /T (Conditional mods to Reflex) /V (%s) >>\n" "" -- TODO
  printf "<< /T (Conditional mods to Will) /V (%s) >>\n" "" -- TODO
  printf "<< /T (Race features - Ability score mods) /V (%s) >>\n" $ racialAbilModifiers c
  printf "<< /T (Race features line 2) /V (%s) >>\n" $ racialFeature 0 c
  printf "<< /T (Race features line 3) /V (%s) >>\n" $ racialFeature 1 c
  printf "<< /T (Race features line 4) /V (%s) >>\n" $ racialFeature 2 c
  printf "<< /T (Race features line 5) /V (%s) >>\n" $ racialFeature 3 c
  printf "<< /T (Race features line 6) /V (%s) >>\n" $ racialFeature 4 c
  printf "<< /T (Race features line 7) /V (%s) >>\n" $ racialFeature 5 c
  printf "<< /T (Race features line 8) /V (%s) >>\n" $ racialFeature 6 c
  printf "<< /T (Race features line 9) /V (%s) >>\n" $ racialFeature 7 c
  printf "<< /T (Str) /V (%d) >>\n" $ str c
  printf "<< /T (Ability mod from Str) /V (%d) >>\n" $ strMod c
  printf "<< /T (Str mod w/ level) /V (%d) >>\n" $ halfLevel c + strMod c -- TODO rename field
  printf "<< /T (Con) /V (%d) >>\n" $ con c
  printf "<< /T (Ability mod from Con) /V (%d) >>\n" $ conMod c
  printf "<< /T (Con mod w/ level) /V (%d) >>\n" $ halfLevel c + conMod c -- TODO rename field
  printf "<< /T (Dex) /V (%d) >>\n" $ dex c
  printf "<< /T (Ability mod from Dex) /V (%d) >>\n" $ dexMod c
  printf "<< /T (Dex mod w/ level) /V (%d) >>\n" $ halfLevel c + dexMod c -- TODO rename field
  printf "<< /T (Int) /V (%d) >>\n" $ int c
  printf "<< /T (Ability mod to Int) /V (%d) >>\n" $ intMod c
  printf "<< /T (Int mod w/ level) /V (%d) >>\n" $ halfLevel c + intMod c -- TODO rename field
  printf "<< /T (Wis) /V (%d) >>\n" $ wis c
  printf "<< /T (Ability mod from Wis) /V (%d) >>\n" $ wisMod c
  printf "<< /T (Wis mod w/ level) /V (%d) >>\n" $ halfLevel c + wisMod c -- TODO rename field
  printf "<< /T (Cha) /V (%d) >>\n" $ cha c
  printf "<< /T (Ability mod from Cha) /V (%d) >>\n" $ chaMod c
  printf "<< /T (Cha mod w/ level) /V (%d) >>\n" $ halfLevel c + chaMod c -- TODO rename field
  printf "<< /T (Speed) /V (%d) >>\n" $ speed c
  printf "<< /T (Base Speed) /V (%d) >>\n" $ baseSpeed c
  printf "<< /T (Armor Speed mod) /V (%d) >>\n" $ armorSpeedMod c
  printf "<< /T (Item Speed mod) /V (%d) >>\n" $ itemSpeedMod c
  printf "<< /T (Misc. Speed mod) /V (%d) >>\n" $ miscModToSpeed c
  printf "<< /T (Special movement) /V (%s) >>\n" "" -- TODO
  printf "<< /T (Trained in Acrobatics) /V (%d) >>\n" $ trainedBonus Acrobatics c
  printf "<< /T (Trained in Arcana) /V (%d) >>\n" $ trainedBonus Arcana c
  printf "<< /T (Trained in Athletics) /V (%d) >>\n" $ trainedBonus Athletics c
  printf "<< /T (Trained in Bluff) /V (%d) >>\n" $ trainedBonus Bluff c
  printf "<< /T (Trained in Diplomacy) /V (%d) >>\n" $ trainedBonus Diplomacy c
  printf "<< /T (Trained in Dungeoneering) /V (%d) >>\n" $ trainedBonus Dungeoneering c
  printf "<< /T (Trained in Endurance) /V (%d) >>\n" $ trainedBonus Endurance c
  printf "<< /T (Trained in Heal) /V (%d) >>\n" $ trainedBonus Heal c
  printf "<< /T (Trained in History) /V (%d) >>\n" $ trainedBonus History c
  printf "<< /T (Trained in Insight) /V (%d) >>\n" $ trainedBonus Insight c
  printf "<< /T (Trained in Intimidate) /V (%d) >>\n" $ trainedBonus Intimidate c
  printf "<< /T (Trained in Nature) /V (%d) >>\n" $ trainedBonus Nature c
  printf "<< /T (Trained in Perception) /V (%d) >>\n" $ trainedBonus Perception c
  printf "<< /T (Trained in Religion) /V (%d) >>\n" $ trainedBonus Religion c
  printf "<< /T (Trained in Stealth) /V (%d) >>\n" $ trainedBonus Stealth c
  printf "<< /T (Trained in Streetwise) /V (%d) >>\n" $ trainedBonus Streetwise c
  printf "<< /T (Trained in Thievery) /V (%d) >>\n" $ trainedBonus Thievery c
  printf "<< /T (Passive Insight) /V (%d) >>\n" $ passiveInsight c
  printf "<< /T (Passive Insight skill mod) /V (%d) >>\n" $ skill Insight c
  printf "<< /T (Passive Perception) /V (%d) >>\n" $ passivePerception c
  printf "<< /T (Passive Perception skill mod) /V (%d) >>\n" $ skill Perception c
  printf "<< /T (Special senses) /V (%s) >>\n" "" -- TODO
  printf "<< /T (Max HP) /V (%d) >>\n" $ hp c
  printf "<< /T (Bloodied) /V (%d) >>\n" $ bloodied c
  printf "<< /T (Surge value) /V (%d) >>\n" $ healingSurgeValue c
  printf "<< /T (Surges per day) /V (%d) >>\n" $ healingSurgesPerDay c
  -- printf "<< /T (Saving throw mods) /V (%s) >>\n" "" -- TODO
  -- printf "<< /T (Resistances) /V (%s) >>\n" "" -- TODO
  -- printf "<< /T (Current conditions and effects) /V (%s) >>\n" "" -- TODO
  printf "<< /T (Class, Path, or Destiny features line 1) /V (%s) >>\n" $ classPathOrDestinyFeature 0 c
  printf "<< /T (Class, Path, or Destiny features line 2) /V (%s) >>\n" $ classPathOrDestinyFeature 1 c
  printf "<< /T (Class, Path, or Destiny features line 3) /V (%s) >>\n" $ classPathOrDestinyFeature 2 c
  printf "<< /T (Class, Path, or Destiny features line 4) /V (%s) >>\n" $ classPathOrDestinyFeature 3 c
  printf "<< /T (Class, Path, or Destiny features line 5) /V (%s) >>\n" $ classPathOrDestinyFeature 4 c
  printf "<< /T (Class, Path, or Destiny features line 6) /V (%s) >>\n" $ classPathOrDestinyFeature 5 c
  printf "<< /T (Class, Path, or Destiny features line 7) /V (%s) >>\n" $ classPathOrDestinyFeature 6 c
  printf "<< /T (Class, Path, or Destiny features line 8) /V (%s) >>\n" $ classPathOrDestinyFeature 7 c
  printf "<< /T (Class, Path, or Destiny features line 9) /V (%s) >>\n" $ classPathOrDestinyFeature 8 c
  printf "<< /T (Class, Path, or Destiny features line 10) /V (%s) >>\n" $ classPathOrDestinyFeature 9 c
  printf "<< /T (Class, Path, or Destiny features line 11) /V (%s) >>\n" $ classPathOrDestinyFeature 10 c
  printf "<< /T (Class, Path, or Destiny features line 12) /V (%s) >>\n" $ classPathOrDestinyFeature 11 c
  printf "<< /T (Class, Path, or Destiny features line 13) /V (%s) >>\n" $ classPathOrDestinyFeature 12 c
  printf "<< /T (Class, Path, or Destiny features line 14) /V (%s) >>\n" $ classPathOrDestinyFeature 13 c
  printf "<< /T (Languages known line 1) /V (%s) >>\n" $ language 0 c
  printf "<< /T (Languages known line 2) /V (%s) >>\n" $ language 1 c
  printf "<< /T (Languages known line 3) /V (%s) >>\n" $ language 2 c
  printf "<< /T (Ability mod plus half-level to Acrobatics) /V (%d) >>\n" $ skillAbilModPlusHalfLevel Acrobatics c
  printf "<< /T (Armor mod to Acrobatics) /V (%d) >>\n" $ skillArmorCheckPenalty Acrobatics c
  printf "<< /T (Misc. mod to Acrobatics) /V (%d) >>\n" $ miscModToSkill Acrobatics c
  printf "<< /T (Acrobatics) /V (%d) >>\n" $ skill Acrobatics c
  printf "<< /T (Misc. mod to Arcana) /V (%d) >>\n" $ miscModToSkill Arcana c
  printf "<< /T (Ability mod plus half-level to Arcana) /V (%d) >>\n" $ skillAbilModPlusHalfLevel Arcana c
  printf "<< /T (Arcana) /V (%d) >>\n" $ skill Arcana c
  printf "<< /T (Ability mod plus half-level to Athletics) /V (%d) >>\n" $ skillAbilModPlusHalfLevel Athletics c
  printf "<< /T (Armor mod to Athletics) /V (%d) >>\n" $ skillArmorCheckPenalty Athletics c
  printf "<< /T (Misc. mod to Athletics) /V (%d) >>\n" $ miscModToSkill Athletics c
  printf "<< /T (Athletics) /V (%d) >>\n" $ skill Athletics c
  printf "<< /T (Ability mod plus half-level to Bluff) /V (%d) >>\n" $ skillAbilModPlusHalfLevel Bluff c
  printf "<< /T (Misc. mod to Bluff) /V (%d) >>\n" $ miscModToSkill Bluff c
  printf "<< /T (Bluff) /V (%d) >>\n" $ skill Bluff c
  printf "<< /T (Ability mod plus half-level to Diplomacy) /V (%d) >>\n" $ skillAbilModPlusHalfLevel Diplomacy c
  printf "<< /T (Misc. mod to Diplomacy) /V (%d) >>\n" $ miscModToSkill Diplomacy c
  printf "<< /T (Diplomacy) /V (%d) >>\n" $ skill Diplomacy c
  printf "<< /T (Ability mod plus half-level to Dungeoneering) /V (%d) >>\n" $ skillAbilModPlusHalfLevel Dungeoneering c
  printf "<< /T (Misc. mod to Dungeoneering) /V (%d) >>\n" $ miscModToSkill Dungeoneering c
  printf "<< /T (Dungeoneering) /V (%d) >>\n" $ skill Dungeoneering c
  printf "<< /T (Armor mod to Endurance) /V (%d) >>\n" $ skillArmorCheckPenalty Endurance c
  printf "<< /T (Misc. mod to Endurance) /V (%d) >>\n" $ miscModToSkill Endurance c
  printf "<< /T (Ability mod plus half-level to Endurance) /V (%d) >>\n" $ skillAbilModPlusHalfLevel Endurance c
  printf "<< /T (Endurance) /V (%d) >>\n" $ skill Endurance c
  printf "<< /T (Ability mod plus half-level to Heal) /V (%d) >>\n" $ skillAbilModPlusHalfLevel Heal c
  printf "<< /T (Misc. mod to Heal) /V (%d) >>\n" $ miscModToSkill Heal c
  printf "<< /T (Ability mod plus half-level to History) /V (%d) >>\n" $ skillAbilModPlusHalfLevel History c
  printf "<< /T (Heal) /V (%d) >>\n" $ skill Heal c
  printf "<< /T (Misc. mod to History) /V (%d) >>\n" $ miscModToSkill History c
  printf "<< /T (History) /V (%d) >>\n" $ skill History c
  printf "<< /T (Insight) /V (%d) >>\n" $ skill Insight c
  printf "<< /T (Misc. mod to Insight) /V (%d) >>\n" $ miscModToSkill Insight c
  printf "<< /T (Ability mod plus half-level to Insight) /V (%d) >>\n" $ skillAbilModPlusHalfLevel Insight c
  printf "<< /T (Intimidate) /V (%d) >>\n" $ skill Intimidate c
  printf "<< /T (Misc. mod to Intimidate) /V (%d) >>\n" $ miscModToSkill Intimidate c
  printf "<< /T (Ability mod plus half-level to Intimidate) /V (%d) >>\n" $ skillAbilModPlusHalfLevel Intimidate c
  printf "<< /T (Nature) /V (%d) >>\n" $ skill Nature c
  printf "<< /T (Misc. mod to Nature) /V (%d) >>\n" $ miscModToSkill Nature c
  printf "<< /T (Ability mod plus half-level to Nature) /V (%d) >>\n" $ skillAbilModPlusHalfLevel Nature c
  printf "<< /T (Perception) /V (%d) >>\n" $ skill Perception c
  printf "<< /T (Misc. mod to Perception) /V (%d) >>\n" $ miscModToSkill Perception c
  printf "<< /T (Misc. mod to Religion) /V (%d) >>\n" $ miscModToSkill Religion c
  printf "<< /T (Religion) /V (%d) >>\n" $ skill Religion c
  printf "<< /T (Ability mod plus half-level to Perception) /V (%d) >>\n" $ skillAbilModPlusHalfLevel Perception c
  printf "<< /T (Ability mod plus half-level to Religion) /V (%d) >>\n" $ skillAbilModPlusHalfLevel Religion c
  printf "<< /T (Stealth) /V (%d) >>\n" $ skill Stealth c
  printf "<< /T (Armor mod to Stealth) /V (%d) >>\n" $ skillArmorCheckPenalty Stealth c
  printf "<< /T (Misc. mod to Stealth) /V (%d) >>\n" $ miscModToSkill Stealth c
  printf "<< /T (Ability mod plus half-level to Stealth) /V (%d) >>\n" $ skillAbilModPlusHalfLevel Stealth c
  printf "<< /T (Streetwise) /V (%d) >>\n" $ skill Streetwise c
  printf "<< /T (Misc. mod to Streetwise) /V (%d) >>\n" $ miscModToSkill Streetwise c
  printf "<< /T (Ability mod plus half-level to Streetwise) /V (%d) >>\n" $ skillAbilModPlusHalfLevel Streetwise c
  printf "<< /T (Thievery) /V (%d) >>\n" $ skill Thievery c
  printf "<< /T (Armor mod to Thievery) /V (%d) >>\n" $ skillArmorCheckPenalty Thievery c
  printf "<< /T (Misc. mod to Thievery) /V (%d) >>\n" $ miscModToSkill Thievery c
  printf "<< /T (Ability mod plus half-level to Thievery) /V (%d) >>\n" $ skillAbilModPlusHalfLevel Thievery c


  printf "<< /T (Attack workspace ability 1) /V (%s) >>\n" $ attack1Name c
  printf "<< /T (Attack mod to attack 1) /V (%d) >>\n" $ attack1Mod c
  printf "<< /T (Half-level to attack 1) /V (%d) >>\n" $ halfLevel c
  printf "<< /T (Ability mod to attack 1) /V (%d) >>\n" $ attack1AbilMod c
  printf "<< /T (Class mod to attack 1) /V (%d) >>\n" $ attack1ClassMod c
  printf "<< /T (Proficiency mod to attack 1) /V (%d) >>\n" $ attack1ProfMod c
  printf "<< /T (Feat mod to attack 1) /V (%d) >>\n" $ attack1FeatMod c
  printf "<< /T (Enhancement mod to attack 1) /V (%d) >>\n" $ attack1EnhMod c
  printf "<< /T (Misc. mod to attack 1) /V (%d) >>\n" $ attack1MiscMod c

  printf "<< /T (Attack workspace ability 2) /V (%s) >>\n" $ attack2Name c
  printf "<< /T (Attack mod to attack 2) /V (%d) >>\n" $ attack2Mod c
  printf "<< /T (Half-level to attack 2) /V (%d) >>\n" $ halfLevel c
  printf "<< /T (Ability mod to attack 2) /V (%d) >>\n" $ attack2AbilMod c
  printf "<< /T (Class mod to attack 2) /V (%d) >>\n" $ attack2ClassMod c
  printf "<< /T (Proficiency mod to attack 2) /V (%d) >>\n" $ attack2ProfMod c
  printf "<< /T (Feat mod to attack 2) /V (%d) >>\n" $ attack2FeatMod c
  printf "<< /T (Enhancement mod to attack 2) /V (%d) >>\n" $ attack2EnhMod c
  printf "<< /T (Misc. mod to attack 2) /V (%d) >>\n" $ attack2MiscMod c

  printf "<< /T (Damage workspace ability 1) /V (%s) >>\n" $ attack1Name c
  printf "<< /T (Damage to ability 1) /V (%s) >>\n" $ damage1Desc c
  printf "<< /T (Ability mod to damage to ability 1) /V (%d) >>\n" $ damage1AbilMod c
  printf "<< /T (Feat mod to damage to ability 1) /V (%d) >>\n" $ damage1FeatMod c
  printf "<< /T (Enhancement mod to damage to ability 1) /V (%d) >>\n" $ damage1EnhMod c
  printf "<< /T (Misc. mod 1 to damage to ability 1) /V (%d) >>\n" $ damage1Misc1Mod c
  printf "<< /T (Misc. mod 2 to damage to ability 1) /V (%d) >>\n" $ damage1Misc2Mod c

  printf "<< /T (Damage workspace ability 2) /V (%s) >>\n" $ attack2Name c
  printf "<< /T (Damage to ability 2) /V (%s) >>\n" $ damage2Desc c
  printf "<< /T (Ability mod to damage to ability 2) /V (%d) >>\n" $ damage2AbilMod c
  printf "<< /T (Feat mod to damage to ability 2) /V (%d) >>\n" $ damage2FeatMod c
  printf "<< /T (Enhancement mod to damage to ability 2) /V (%d) >>\n" $ damage2EnhMod c
  printf "<< /T (Misc. mod 1 to damage to ability 2) /V (%d) >>\n" $ damage2Misc1Mod c
  printf "<< /T (Misc. mod 2 to damage to ability 2) /V (%d) >>\n" $ damage2Misc2Mod c

  -- printf "<< /T (Basic attack 1 attack) /V (%d) >>\n" (0 :: Int)
  -- printf "<< /T (Basic attack 1 defense) /V (%s) >>\n" ""
  -- printf "<< /T (Basic attack 2 attack) /V (%s) >>\n" ""
  -- printf "<< /T (Basic attack 2 defense) /V (%s) >>\n" ""
  -- printf "<< /T (Basic attack 3 attack ) /V (%s) >>\n" ""
  -- printf "<< /T (Basic attack 3 defense) /V (%s) >>\n" ""
  -- printf "<< /T (Basic attack 4 attack ) /V (%s) >>\n" ""
  -- printf "<< /T (Basic attack 4 defense) /V (%s) >>\n" ""
  -- printf "<< /T (Basic attack 1 weapon or power) /V (%s) >>\n" ""
  -- printf "<< /T (Basic attack 2 weapon or power) /V (%s) >>\n" ""
  -- printf "<< /T (Basic attack 3 weapon or power) /V (%s) >>\n" ""
  -- printf "<< /T (Basic attack 4 weapon or power) /V (%s) >>\n" ""
  -- printf "<< /T (Basic attack 1 damage) /V (%s) >>\n" ""
  -- printf "<< /T (Basic attack 2 damage) /V (%s) >>\n" ""
  -- printf "<< /T (Basic attack 3 damage) /V (%s) >>\n" ""
  -- printf "<< /T (Basic attack 4 damage) /V (%s) >>\n" ""
  printf "<< /T (Feats line 1) /V (%s) >>\n" $ feat 0 c
  printf "<< /T (Feats line 2) /V (%s) >>\n" $ feat 1 c
  printf "<< /T (Feats line 3) /V (%s) >>\n" $ feat 2 c
  printf "<< /T (Feats line 4) /V (%s) >>\n" $ feat 3 c
  printf "<< /T (Feats line 5) /V (%s) >>\n" $ feat 4 c
  printf "<< /T (Feats line 6) /V (%s) >>\n" $ feat 5 c
  printf "<< /T (Feats line 7) /V (%s) >>\n" $ feat 6 c
  printf "<< /T (Feats line 8) /V (%s) >>\n" $ feat 7 c
  printf "<< /T (Feats line 9) /V (%s) >>\n" $ feat 8 c
  printf "<< /T (Feats line 10) /V (%s) >>\n" $ feat 9 c
  printf "<< /T (Feats line 11) /V (%s) >>\n" $ feat 10 c
  printf "<< /T (Feats line 12) /V (%s) >>\n" $ feat 11 c
  printf "<< /T (Feats line 13) /V (%s) >>\n" $ feat 12 c
  printf "<< /T (Feats line 14) /V (%s) >>\n" $ feat 13 c
  printf "<< /T (Feats line 15) /V (%s) >>\n" $ feat 14 c
  printf "<< /T (Feats line 16) /V (%s) >>\n" $ feat 15 c
  printf "<< /T (Feats line 17) /V (%s) >>\n" $ feat 16 c
  printf "<< /T (At-will powers line 1) /V (%s) >>\n" $ atWillPower 0 c
  printf "<< /T (At-will powers line 2) /V (%s) >>\n" $ atWillPower 1 c
  printf "<< /T (At-will powers line 3) /V (%s) >>\n" $ atWillPower 2 c
  printf "<< /T (At-will powers line 4) /V (%s) >>\n" $ atWillPower 3 c
  printf "<< /T (At-will powers line 5) /V (%s) >>\n" $ atWillPower 4 c
  printf "<< /T (At-will powers line 6) /V (%s) >>\n" $ atWillPower 5 c
  printf "<< /T (Daily powers line 1) /V (%s) >>\n" $ dailyPower 0 c
  printf "<< /T (Daily powers line 2) /V (%s) >>\n" $ dailyPower 1 c
  printf "<< /T (Daily powers line 3) /V (%s) >>\n" $ dailyPower 2 c
  printf "<< /T (Daily powers line 4) /V (%s) >>\n" $ dailyPower 3 c
  printf "<< /T (Daily powers line 5) /V (%s) >>\n" $ dailyPower 4 c
  printf "<< /T (Daily powers line 6) /V (%s) >>\n" $ dailyPower 5 c
  printf "<< /T (Encounter powers line 1) /V (%s) >>\n" $ encounterPower 0 c
  printf "<< /T (Encounter powers line 2) /V (%s) >>\n" $ encounterPower 1 c
  printf "<< /T (Encounter powers line 3) /V (%s) >>\n" $ encounterPower 2 c
  printf "<< /T (Encounter powers line 4) /V (%s) >>\n" $ encounterPower 3 c
  printf "<< /T (Encounter powers line 5) /V (%s) >>\n" $ encounterPower 4 c
  printf "<< /T (Encounter powers line 6) /V (%s) >>\n" $ encounterPower 5 c
  printf "<< /T (Utility powers line 1) /V (%s) >>\n"  $ utilityPower 0 c
  printf "<< /T (Utility powers line 2) /V (%s) >>\n"  $ utilityPower 1 c
  printf "<< /T (Utility powers line 3) /V (%s) >>\n"  $ utilityPower 2 c
  printf "<< /T (Utility powers line 4) /V (%s) >>\n"  $ utilityPower 3 c
  printf "<< /T (Utility powers line 5) /V (%s) >>\n"  $ utilityPower 4 c
  printf "<< /T (Utility powers line 6) /V (%s) >>\n"  $ utilityPower 5 c
  printf "<< /T (Utility powers line 7) /V (%s) >>\n"  $ utilityPower 6 c
  printf "<< /T (Utility powers line 8) /V (%s) >>\n"  $ utilityPower 7 c
  printf "<< /T (Magic items line 1) /V (%s) >>\n" $ magicItem 0 c -- TODO weapon
  printf "<< /T (Magic items line 2) /V (%s) >>\n" $ magicItem 1 c -- TODO weapon
  printf "<< /T (Magic items line 3) /V (%s) >>\n" $ magicItem 2 c -- TODO weapon
  printf "<< /T (Magic items line 4) /V (%s) >>\n" $ magicItem 3 c -- TODO weapon
  printf "<< /T (Magic items line 5) /V (%s) >>\n" $ magicItem 4 c -- TODO weapon
  printf "<< /T (Magic items line 6) /V (%s) >>\n" $ magicItem 5 c -- TODO armor
  printf "<< /T (Magic items line 7) /V (%s) >>\n" $ magicItem 6 c -- TODO arms
  printf "<< /T (Magic items line 8) /V (%s) >>\n" $ magicItem 7 c -- TODO feet
  printf "<< /T (Magic items line 9) /V (%s) >>\n" $ magicItem 8 c -- TODO hands
  printf "<< /T (Magic items line 10) /V (%s) >>\n" $ magicItem 9 c -- TODO head
  printf "<< /T (Magic items line 11) /V (%s) >>\n" $ magicItem 10 c -- TODO neck
  printf "<< /T (Magic items line 12) /V (%s) >>\n" $ magicItem 11 c -- TODO ring
  printf "<< /T (Magic items line 13) /V (%s) >>\n" $ magicItem 12 c -- TODO ring
  printf "<< /T (Magic items line 14) /V (%s) >>\n" $ magicItem 13 c -- TODO waist
  printf "<< /T (Magic items line 15) /V (%s) >>\n" $ magicItem 14 c
  printf "<< /T (Magic items line 16) /V (%s) >>\n" $ magicItem 15 c
  printf "<< /T (Magic items line 17) /V (%s) >>\n" $ magicItem 16 c
  printf "<< /T (Magic items line 18) /V (%s) >>\n" $ magicItem 17 c
  printf "<< /T (Magic items line 19) /V (%s) >>\n" $ magicItem 18 c
  printf "<< /T (Magic items line 20) /V (%s) >>\n" $ magicItem 19 c
  printf "<< /T (Magic items line 21) /V (%s) >>\n" $ magicItem 20 c
  printf "<< /T (Magic items line 22) /V (%s) >>\n" $ magicItem 21 c
  printf "<< /T (Magic items line 23) /V (%s) >>\n" $ magicItem 22 c
  printf "<< /T (Magic items line 24) /V (%s) >>\n" $ magicItem 23 c
  printf "<< /T (Magic items line 25) /V (%s) >>\n" $ magicItem 24 c
  -- printf "<< /T (Other equipment line 1) /V (%s) >>\n" ""
  -- printf "<< /T (Other equipment line 2) /V (%s) >>\n" ""
  -- printf "<< /T (Other equipment line 3) /V (%s) >>\n" ""
  -- printf "<< /T (Other equipment line 4) /V (%s) >>\n" ""
  -- printf "<< /T (Other equipment line 5) /V (%s) >>\n" ""
  -- printf "<< /T (Other equipment line 6) /V (%s) >>\n" ""
  -- printf "<< /T (Other equipment line 7) /V (%s) >>\n" ""
  -- printf "<< /T (Other equipment line 8) /V (%s) >>\n" ""
  -- printf "<< /T (Other equipment line 9) /V (%s) >>\n" ""
  -- printf "<< /T (Other equipment line 10) /V (%s) >>\n" ""
  -- printf "<< /T (Rituals line 1) /V (%s) >>\n" ""
  -- printf "<< /T (Rituals line 2) /V (%s) >>\n" ""
  -- printf "<< /T (Rituals line 3) /V (%s) >>\n" ""
  -- printf "<< /T (Rituals line 4) /V (%s) >>\n" ""
  -- printf "<< /T (Rituals line 5) /V (%s) >>\n" ""
  -- printf "<< /T (Rituals line 6) /V (%s) >>\n" ""
  -- printf "<< /T (Rituals line 7) /V (%s) >>\n" ""
  -- printf "<< /T (Rituals line 8) /V (%s) >>\n" ""
  -- printf "<< /T (Rituals line 9) /V (%s) >>\n" ""
  -- printf "<< /T (Rituals line 10) /V (%s) >>\n" ""
  -- printf "<< /T (Coins and other wealth) /V (%s) >>\n" ""
  -- printf "<< /T (Personality traits line 1) /V (%s) >>\n" ""
  -- printf "<< /T (Personality traits line 2) /V (%s) >>\n" ""
  -- printf "<< /T (Personality traits line 3) /V (%s) >>\n" ""
  -- printf "<< /T (Personality traits line 5) /V (%s) >>\n" ""
  -- printf "<< /T (Personality traits line 4) /V (%s) >>\n" ""
  -- printf "<< /T (Personality traits line 6) /V (%s) >>\n" ""
  -- printf "<< /T (Personality traits line 7) /V (%s) >>\n" ""
  -- printf "<< /T (Personality traits line 8) /V (%s) >>\n" ""
  -- printf "<< /T (Personality traits line 9) /V (%s) >>\n" ""
  -- printf "<< /T (Personality traits line 10) /V (%s) >>\n" ""
  -- printf "<< /T (Personality traits line 11) /V (%s) >>\n" ""
  -- printf "<< /T (Character background line 1) /V (%s) >>\n" ""
  -- printf "<< /T (Character background line 2) /V (%s) >>\n" ""
  -- printf "<< /T (Character background line 3) /V (%s) >>\n" ""
  -- printf "<< /T (Character background line 4) /V (%s) >>\n" ""
  -- printf "<< /T (Companions name 1) /V (%s) >>\n" ""
  -- printf "<< /T (Companions notes 1) /V (%s) >>\n" ""
  -- printf "<< /T (Companions name 2) /V (%s) >>\n" ""
  -- printf "<< /T (Companions notes 2) /V (%s) >>\n" ""
  -- printf "<< /T (Companions name 3) /V (%s) >>\n" ""
  -- printf "<< /T (Companions notes 3) /V (%s) >>\n" ""
  -- printf "<< /T (Companions name 4) /V (%s) >>\n" ""
  -- printf "<< /T (Companions notes 4) /V (%s) >>\n" ""
  -- printf "<< /T (Companions name 5) /V (%s) >>\n" ""
  -- printf "<< /T (Companions notes 5) /V (%s) >>\n" ""
  -- printf "<< /T (Companions name 6) /V (%s) >>\n" ""
  -- printf "<< /T (Companions notes 6) /V (%s) >>\n" ""
  -- printf "<< /T (Companions name 7) /V (%s) >>\n" ""
  -- printf "<< /T (Companions notes 7) /V (%s) >>\n" ""
  -- printf "<< /T (Companions name 8) /V (%s) >>\n" ""
  -- printf "<< /T (Companions notes 8) /V (%s) >>\n" ""
  -- printf "<< /T (Session and campaign notes 1) /V (%s) >>\n" ""

main = do
  printFdfHeader
  printFdfData pontus
  printFdfFooter