module Form where

import Text.Printf
import Modifier
import Race
import Character as C
import Skill
import Ability
import Power
import Feat
import Pontus


-- Consider a function that if the resulting int is 0, outputs nothing, so
-- that we can write in values on the printed sheet.
printFormData c = do
  putStrLn "$fields = {"
  printf "  'Player name' => q{%s},\n" $ C.playerName c
  printf "  'Character name' => q{%s},\n" $ C.name c
  printf "  'Level' => q{%d},\n" $ C.level c
  printf "  'Class' => q{%s},\n" $ C.className c
  printf "  'Paragon path' => q{%s},\n" "" -- TODO
  printf "  'Epic destiny' => q{%s},\n" "" -- TODO
  printf "  'Total XP' => q{%d},\n" $ C.xp c
  printf "  'Race' => q{%s},\n" $ (Race.name . C.race) c
  printf "  'Size' => q{%s},\n" $ (Race.size . C.race) c
  printf "  'Age' => q{%d},\n" $ C.age c
  printf "  'Gender' => q{%s},\n" $ C.gender c
  printf "  'Height' => q{%s},\n" $ C.height c
  printf "  'Weight' => q{%s},\n" $ C.weight c
  printf "  'Alignment' => q{%s},\n" $ C.alignment c
  printf "  'Deity' => q{%s},\n" $ C.deity c
  printf "  'Adventuring company or other affiliation' => q{%s},\n" $ C.adventuringCompanyOrOtherAffiliations c
  printf "  'Dex mod to Init' => q{%d},\n" $ C.dexMod c
  printf "  'Half-level mod to Init' => q{%d},\n" $ C.halfLevel c
  printf "  'Misc. mod to Init' => q{%d},\n" $ C.miscModToInit c
  printf "  'Init' => q{%d},\n" $ C.initiative c
  printf "  'Conditional Init mods' => q{%s},\n" "" -- TODO
  printf "  'Ten plus half-level mod to AC' => q{%d},\n" $ C.tenPlusHalfLevel c
  printf "  'Armor or ability mod to AC' => q{%d},\n" $ C.armorOrAbilityModToAC c
  printf "  'Class mod to AC' => q{%d},\n" $ C.classModToAC c
  printf "  'Feat mod to AC' => q{%d},\n" $ C.featModToAC c
  printf "  'Enhancement mod to AC' => q{%d},\n" $ C.enhModToAC c
  printf "  'Misc. mod 1 to AC' => q{%d},\n" $ C.firstMiscACMod c
  printf "  'Misc. mod 2 to AC' => q{%d},\n" $ C.secondMiscACMod c
  printf "  'AC' => q{%d},\n" $ C.ac c
  printf "  'Ten plus half-level to Fortitude' => q{%d},\n" $ C.tenPlusHalfLevel c
  printf "  'Ability mod to Fortitude' => q{%d},\n" $ C.abilityModToFortitude c
  printf "  'Class mod to Fortitude' => q{%d},\n" $ C.classModToFortitude c
  printf "  'Feat mod to Fortitude' => q{%d},\n" $ C.featModToFortitude c
  printf "  'Enhancement mod to Fortitude' => q{%d},\n" $ C.enhModToFortitude c
  printf "  'Misc. mod 1 to Fortitude' => q{%d},\n" $ C.firstMiscFortitudeMod c
  printf "  'Misc. mod 2 to Fortitude' => q{%d},\n" $ C.secondMiscFortitudeMod c
  printf "  'Fortitude' => q{%d},\n" $ C.fortitude c
  printf "  'Ten plus half-level to Reflex' => q{%d},\n" $ C.tenPlusHalfLevel c
  printf "  'Ability mod to Reflex' => q{%d},\n" $ C.abilityModToReflex c
  printf "  'Class mod to Reflex' => q{%d},\n" $ C.classModToReflex c
  printf "  'Feat mod to Reflex' => q{%d},\n" $ C.featModToReflex c
  printf "  'Enhancement mod to Reflex' => q{%d},\n" $ C.enhModToReflex c
  printf "  'Misc. mod 1 to Reflex' => q{%d},\n" $ C.firstMiscReflexMod c
  printf "  'Misc. mod 2 to Reflex' => q{%d},\n" $ C.secondMiscReflexMod c
  printf "  'Reflex' => q{%d},\n" $ C.reflex c
  printf "  'Ten plus half-level to Will' => q{%d},\n" $ C.tenPlusHalfLevel c
  printf "  'Ability mod to Will' => q{%d},\n" $ C.abilityModToWill c
  printf "  'Class mod to Will' => q{%d},\n" $ C.classModToWill c
  printf "  'Feat mod to Will' => q{%d},\n" $ C.featModToWill c
  printf "  'Enhancement mod to Will' => q{%d},\n" $ C.enhModToWill c
  printf "  'Misc. mod 1 to Will' => q{%d},\n" $ C.firstMiscWillMod c
  printf "  'Misc. mod 2 to Will' => q{%d},\n" $ C.secondMiscWillMod c
  printf "  'Will' => q{%d},\n" $ C.will c
  printf "  'Additional effects to spending action points' => q{%s},\n" ""
  printf "  'Conditional mods to AC' => q{%s},\n" ""
  printf "  'Conditional mods to Fortitude' => q{%s},\n" ""
  printf "  'Conditional mods to Reflex' => q{%s},\n" ""
  printf "  'Conditional mods to Will' => q{%s},\n" ""
  printf "  'Race features - Ability score mods' => q{%s},\n" ""
  printf "  'Race features line 2' => q{%s},\n" ""
  printf "  'Race features line 3' => q{%s},\n" ""
  printf "  'Race features line 4' => q{%s},\n" ""
  printf "  'Race features line 5' => q{%s},\n" ""
  printf "  'Race features line 6' => q{%s},\n" ""
  printf "  'Race features line 7' => q{%s},\n" ""
  printf "  'Race features line 8' => q{%s},\n" ""
  printf "  'Race features line 9' => q{%s},\n" ""
  printf "  'Str' => q{%d},\n" $ C.str c
  printf "  'Ability mod from Str' => q{%d},\n" $ C.strMod c
  printf "  'Str mod w/ level' => q{%d},\n" $ C.halfLevel c + C.strMod c
  printf "  'Con' => q{%d},\n" $ C.con c
  printf "  'Ability mod from Con' => q{%d},\n" $ C.conMod c
  printf "  'Con mod w/ level' => q{%d},\n" $ C.halfLevel c + C.conMod c
  printf "  'Dex' => q{%d},\n" $ C.dex c
  printf "  'Ability mod from Dex' => q{%d},\n" $ C.dexMod c
  printf "  'Dex mod w/ level' => q{%d},\n" $ C.halfLevel c + C.dexMod c
  printf "  'Int' => q{%d},\n" $ C.int c
  printf "  'Ability mod to Int' => q{%d},\n" $ C.intMod c
  printf "  'Int mod w/ level' => q{%d},\n" $ C.halfLevel c + C.intMod c
  printf "  'Wis' => q{%d},\n" $ C.wis c
  printf "  'Ability mod from Wis' => q{%d},\n" $ C.wisMod c
  printf "  'Wis mod w/ level' => q{%d},\n" $ C.halfLevel c + C.wisMod c
  printf "  'Cha' => q{%d},\n" $ C.cha c
  printf "  'Ability mod from Cha' => q{%d},\n" $ C.chaMod c
  printf "  'Cha mod w/ level' => q{%d},\n" $ C.halfLevel c + C.chaMod c
  printf "  'Speed' => q{%d},\n" $ C.speed c
  printf "  'Base Speed' => q{%d},\n" $ (Race.baseSpeed . C.race) c
  printf "  'Armor Speed mod' => q{%d},\n" $ C.armorSpeedMod c
  printf "  'Item Speed mod' => q{%d},\n" $ C.itemSpeedMod c
  printf "  'Misc. Speed mod' => q{%d},\n" $ C.miscSpeedMod c
  printf "  'Special movement' => q{%s},\n" ""
  printf "  'Trained in Acrobatics' => q{%d},\n" $ trainedBonus c Acrobatics
  printf "  'Trained in Arcana' => q{%d},\n" $ trainedBonus c Arcana
  printf "  'Trained in Athletics' => q{%d},\n" $ trainedBonus c Athletics
  printf "  'Trained in Bluff' => q{%d},\n" $ trainedBonus c Bluff
  printf "  'Trained in Diplomacy' => q{%d},\n" $ trainedBonus c Diplomacy
  printf "  'Trained in Dungeoneering' => q{%d},\n" $ trainedBonus c Dungeoneering
  printf "  'Trained in Endurance' => q{%d},\n" $ trainedBonus c Endurance
  printf "  'Trained in Heal' => q{%d},\n" $ trainedBonus c Heal
  printf "  'Trained in History' => q{%d},\n" $ trainedBonus c History
  printf "  'Trained in Insight' => q{%d},\n" $ trainedBonus c Insight
  printf "  'Trained in Intimidate' => q{%d},\n" $ trainedBonus c Intimidate
  printf "  'Trained in Nature' => q{%d},\n" $ trainedBonus c Nature
  printf "  'Trained in Perception' => q{%d},\n" $ trainedBonus c Perception
  printf "  'Trained in Religion' => q{%d},\n" $ trainedBonus c Religion
  printf "  'Trained in Stealth' => q{%d},\n" $ trainedBonus c Stealth
  printf "  'Trained in Streetwise' => q{%d},\n" $ trainedBonus c Streetwise
  printf "  'Trained in Thievery' => q{%d},\n" $ trainedBonus c Thievery
  printf "  'Ability mod plus half-level to Acrobatics' => q{%d},\n" $ skillAbilModPlusHalfLevel c Acrobatics
  printf "  'Armor mod to Acrobatics' => q{%d},\n" $ skillArmorCheckPenalty c Acrobatics
  printf "  'Misc. mod to Acrobatics' => q{%s},\n" ""
  printf "  'Acrobatics' => q{%d},\n" $ skill c Acrobatics
  printf "  'Passive Insight' => q{%d},\n" $ passiveInsight c
  printf "  'Passive Insight skill mod' => q{%d},\n" $ skill c Insight
  printf "  'Passive Perception' => q{%d},\n" $ passivePerception c
  printf "  'Passive Perception skill mod' => q{%d},\n" $ skill c Perception
  printf "  'Special senses' => q{%s},\n" ""
  printf "  'Max HP' => q{%d},\n" $ C.hp c
  printf "  'Bloodied' => q{%d},\n" $ C.bloodied c
  printf "  'Surge value' => q{%d},\n" $ C.healingSurgeValue c
  printf "  'Surges per day' => q{%d},\n" $ C.healingSurgesPerDay c
  printf "  'Saving throw mods' => q{%s},\n" ""
  printf "  'Resistances' => q{%s},\n" ""
  printf "  'Current conditions and effects' => q{%s},\n" ""
  printf "  'Class, Path, or Destiny features line 1' => q{%s},\n" ""
  printf "  'Class, Path, or Destiny features line 2' => q{%s},\n" ""
  printf "  'Class, Path, or Destiny features line 3' => q{%s},\n" ""
  printf "  'Class, Path, or Destiny features line 4' => q{%s},\n" ""
  printf "  'Class, Path, or Destiny features line 5' => q{%s},\n" ""
  printf "  'Class, Path, or Destiny features line 6' => q{%s},\n" ""
  printf "  'Class, Path, or Destiny features line 7' => q{%s},\n" ""
  printf "  'Class, Path, or Destiny features line 8' => q{%s},\n" ""
  printf "  'Class, Path, or Destiny features line 9' => q{%s},\n" ""
  printf "  'Class, Path, or Destiny features line 10' => q{%s},\n" ""
  printf "  'Class, Path, or Destiny features line 11' => q{%s},\n" ""
  printf "  'Class, Path, or Destiny features line 12' => q{%s},\n" ""
  printf "  'Class, Path, or Destiny features line 13' => q{%s},\n" ""
  printf "  'Class, Path, or Destiny features line 14' => q{%s},\n" ""
  printf "  'Languages known line 1' => q{%s},\n" $ firstLanguage c
  printf "  'Languages known line 2' => q{%s},\n" $ secondLanguage c
  printf "  'Languages known line 3' => q{%s},\n" $ thirdLanguage c
  printf "  'Misc. mod to Arcana' => q{%s},\n" ""
  printf "  'Ability mod plus half-level to Arcana' => q{%d},\n" $ skillAbilModPlusHalfLevel c Arcana
  printf "  'Arcana' => q{%d},\n" $ skill c Arcana
  printf "  'Ability mod plus half-level to Athletics' => q{%d},\n" $ skillAbilModPlusHalfLevel c Athletics
  printf "  'Armor mod to Athletics' => q{%d},\n" $ skillArmorCheckPenalty c Athletics
  printf "  'Misc. mod to Athletics' => q{%s},\n" ""
  printf "  'Athletics' => q{%d},\n" $ skill c Athletics
  printf "  'Ability mod plus half-level to Bluff' => q{%d},\n" $ skillAbilModPlusHalfLevel c Bluff
  printf "  'Misc. mod to Bluff' => q{%s},\n" ""
  printf "  'Bluff' => q{%d},\n" $ skill c Bluff
  printf "  'Ability mod plus half-level to Diplomacy' => q{%d},\n" $ skillAbilModPlusHalfLevel c Diplomacy
  printf "  'Misc. mod to Diplomacy' => q{%s},\n" ""
  printf "  'Diplomacy' => q{%d},\n" $ skill c Diplomacy
  printf "  'Ability mod plus half-level to Dungeoneering' => q{%d},\n" $ skillAbilModPlusHalfLevel c Dungeoneering
  printf "  'Misc. mod to Dungeoneering' => q{%s},\n" ""
  printf "  'Dungeoneering' => q{%d},\n" $ skill c Dungeoneering
  printf "  'Armor mod to Endurance' => q{%d},\n" $ skillArmorCheckPenalty c Endurance
  printf "  'Misc. mod to Endurance' => q{%s},\n" ""
  printf "  'Ability mod plus half-level to Endurance' => q{%d},\n" $ skillAbilModPlusHalfLevel c Endurance
  printf "  'Endurance' => q{%d},\n" $ skill c Endurance
  printf "  'Ability mod plus half-level to Heal' => q{%d},\n" $ skillAbilModPlusHalfLevel c Heal
  printf "  'Misc. mod to Heal' => q{%s},\n" ""
  printf "  'Ability mod plus half-level to History' => q{%d},\n" $ skillAbilModPlusHalfLevel c History
  printf "  'Heal' => q{%d},\n" $ skill c Heal
  printf "  'Misc. mod to History' => q{%s},\n" ""
  printf "  'History' => q{%d},\n" $ skill c History
  printf "  'Insight' => q{%d},\n" $ skill c Insight
  printf "  'Misc. mod to Insight' => q{%s},\n" ""
  printf "  'Ability mod plus half-level to Insight' => q{%d},\n" $ skillAbilModPlusHalfLevel c Insight
  printf "  'Intimidate' => q{%d},\n" $ skill c Intimidate
  printf "  'Misc. mod to Intimidate' => q{%s},\n" ""
  printf "  'Ability mod plus half-level to Intimidate' => q{%d},\n" $ skillAbilModPlusHalfLevel c Intimidate
  printf "  'Nature' => q{%d},\n" $ skill c Nature
  printf "  'Misc. mod to Nature' => q{%s},\n" ""
  printf "  'Ability mod plus half-level to Nature' => q{%d},\n" $ skillAbilModPlusHalfLevel c Nature
  printf "  'Perception' => q{%d},\n" $ skill c Perception
  printf "  'Misc. mod to Perception' => q{%s},\n" ""
  printf "  'Misc. mod to Religion' => q{%s},\n" ""
  printf "  'Religion' => q{%d},\n" $ skill c Religion
  printf "  'Ability mod plus half-level to Perception' => q{%d},\n" $ skillAbilModPlusHalfLevel c Perception
  printf "  'Ability mod plus half-level to Religion' => q{%d},\n" $ skillAbilModPlusHalfLevel c Religion
  printf "  'Stealth' => q{%d},\n" $ skill c Stealth
  printf "  'Armor mod to Stealth' => q{%d},\n" $ skillArmorCheckPenalty c Stealth
  printf "  'Misc. mod to Stealth' => q{%s},\n" ""
  printf "  'Ability mod plus half-level to Stealth' => q{%d},\n" $ skillAbilModPlusHalfLevel c Stealth
  printf "  'Streetwise' => q{%d},\n" $ skill c Streetwise
  printf "  'Misc. mod to Streetwise' => q{%s},\n" ""
  printf "  'Ability mod plus half-level to Streetwise' => q{%d},\n" $ skillAbilModPlusHalfLevel c Streetwise
  printf "  'Thievery' => q{%d},\n" $ skill c Thievery
  printf "  'Armor mod to Thievery' => q{%d},\n" $ skillArmorCheckPenalty c Thievery
  printf "  'Misc. mod to Thievery' => q{%s},\n" ""
  printf "  'Ability mod plus half-level to Thievery' => q{%d},\n" $ skillAbilModPlusHalfLevel c Thievery
  printf "  'Attack workspace ability 1' => q{%s},\n" $ Power.name $ C.firstAttack c
  printf "  'Attack mod to attack 1' => q{%d},\n" (0 :: Int)
  printf "  'Half-level to attack 1' => q{%d},\n" $ halfLevel c
  printf "  'Ability mod to attack 1' => q{%d},\n" $ (C.abilityMod $ Power.attackAbility $ C.firstAttack c) pontus
  printf "  'Class mod to attack 1' => q{%s},\n" "?"
  printf "  'Proficiency mod to attack 1' => q{%d},\n" $ C.proficiencyModForPower c $ C.firstAttack c
  printf "  'Feat mod to attack 1' => q{%s},\n" "?"
  printf "  'Enhancement mod to attack 1' => q{%d},\n" (0 :: Int)
  printf "  'Misc. mod to attack 1' => q{%s},\n" ""
  printf "  'Attack workspace ability 2' => q{%s},\n" $ Power.name $ C.secondAttack c
  printf "  'Attack mod to attack 2' => q{%d},\n" (0 :: Int)
  printf "  'Half-level to attack 2' => q{%d},\n" $ halfLevel c
  printf "  'Ability mod to attack 2' => q{%d},\n" $ (C.abilityMod $ Power.attackAbility $ C.secondAttack c) c
  printf "  'Class mod to attack 2' => q{%d},\n" (0 :: Int)
  printf "  'Proficiency mod to attack 2' => q{%d},\n" $ C.proficiencyModForPower c $ C.secondAttack c
  printf "  'Feat mod to attack 2' => q{%d},\n" (0 :: Int)
  printf "  'Enhancement mod to attack 2' => q{%d},\n" (0 :: Int)
  printf "  'Misc. mod to attack 2' => q{%s},\n" ""
  printf "  'Damage workspace ability 1' => q{%s},\n" ""
  printf "  'Damage workspace ability 2' => q{%s},\n" ""
  printf "  'Damage to ability 1' => q{%s},\n" ""
  printf "  'Ability mod to damage to ability 1' => q{%d},\n" (0 :: Int)
  printf "  'Feat mod to damage to ability 1' => q{%d},\n" (0 :: Int)
  printf "  'Enhancement mod to damage to ability 1' => q{%d},\n" (0 :: Int)
  printf "  'Misc. mod 1 to damage to ability 1' => q{%d},\n" (0 :: Int)
  printf "  'Misc. mod 2 to damage to ability 1' => q{%d},\n" (0 :: Int)
  printf "  'Damage to ability 2' => q{%s},\n" ""
  printf "  'Ability mod to damage to ability 2' => q{%d},\n" (0 :: Int)
  printf "  'Feat mod to damage to ability 2' => q{%d},\n" (0 :: Int)
  printf "  'Enhancement mod to damage to ability 2' => q{%d},\n" (0 :: Int)
  printf "  'Misc. mod 1 to damage to ability 2' => q{%d},\n" (0 :: Int)
  printf "  'Misc. mod 2 to damage to ability 2' => q{%d},\n" (0 :: Int)
  printf "  'Basic attack 1 attack' => q{%d},\n" (0 :: Int)
  printf "  'Basic attack 1 defense' => q{%s},\n" ""
  printf "  'Basic attack 2 attack' => q{%s},\n" ""
  printf "  'Basic attack 2 defense' => q{%s},\n" ""
  printf "  'Basic attack 3 attack ' => q{%s},\n" ""
  printf "  'Basic attack 3 defense' => q{%s},\n" ""
  printf "  'Basic attack 4 attack ' => q{%s},\n" ""
  printf "  'Basic attack 4 defense' => q{%s},\n" ""
  printf "  'Basic attack 1 weapon or power' => q{%s},\n" ""
  printf "  'Basic attack 2 weapon or power' => q{%s},\n" ""
  printf "  'Basic attack 3 weapon or power' => q{%s},\n" ""
  printf "  'Basic attack 4 weapon or power' => q{%s},\n" ""
  printf "  'Basic attack 1 damage' => q{%s},\n" ""
  printf "  'Basic attack 2 damage' => q{%s},\n" ""
  printf "  'Basic attack 3 damage' => q{%s},\n" ""
  printf "  'Basic attack 4 damage' => q{%s},\n" ""
  -- iterate over me somehow
  printf "  'Feats line 1' => q{%s},\n" $ (seventeenFeats c) !! 0
  printf "  'Feats line 2' => q{%s},\n" $ (seventeenFeats c) !! 1
  printf "  'Feats line 3' => q{%s},\n" $ (seventeenFeats c) !! 2
  printf "  'Feats line 4' => q{%s},\n" $ (seventeenFeats c) !! 3
  printf "  'Feats line 5' => q{%s},\n" $ (seventeenFeats c) !! 4
  printf "  'Feats line 6' => q{%s},\n" $ (seventeenFeats c) !! 5
  printf "  'Feats line 7' => q{%s},\n" $ (seventeenFeats c) !! 6
  printf "  'Feats line 8' => q{%s},\n" $ (seventeenFeats c) !! 7
  printf "  'Feats line 9' => q{%s},\n" $ (seventeenFeats c) !! 8
  printf "  'Feats line 10' => q{%s},\n" $ (seventeenFeats c) !! 9
  printf "  'Feats line 11' => q{%s},\n" $ (seventeenFeats c) !! 10
  printf "  'Feats line 12' => q{%s},\n" $ (seventeenFeats c) !! 11
  printf "  'Feats line 13' => q{%s},\n" $ (seventeenFeats c) !! 12
  printf "  'Feats line 14' => q{%s},\n" $ (seventeenFeats c) !! 13
  printf "  'Feats line 15' => q{%s},\n" $ (seventeenFeats c) !! 14
  printf "  'Feats line 16' => q{%s},\n" $ (seventeenFeats c) !! 15
  printf "  'Feats line 17' => q{%s},\n" $ (seventeenFeats c) !! 16
  printf "  'At-will powers line 1' => q{%s},\n" $ (sixAtWillPowers c) !! 0
  printf "  'At-will powers line 2' => q{%s},\n" $ (sixAtWillPowers c) !! 1
  printf "  'At-will powers line 3' => q{%s},\n" $ (sixAtWillPowers c) !! 2
  printf "  'At-will powers line 4' => q{%s},\n" $ (sixAtWillPowers c) !! 3
  printf "  'At-will powers line 5' => q{%s},\n" $ (sixAtWillPowers c) !! 4
  printf "  'At-will powers line 6' => q{%s},\n" $ (sixAtWillPowers c) !! 5
  printf "  'Daily powers line 1' => q{%s},\n" $ (sixDailyPowers c) !! 0
  printf "  'Daily powers line 2' => q{%s},\n" $ (sixDailyPowers c) !! 1
  printf "  'Daily powers line 3' => q{%s},\n" $ (sixDailyPowers c) !! 2
  printf "  'Daily powers line 4' => q{%s},\n" $ (sixDailyPowers c) !! 3
  printf "  'Daily powers line 5' => q{%s},\n" $ (sixDailyPowers c) !! 4
  printf "  'Daily powers line 6' => q{%s},\n" $ (sixDailyPowers c) !! 5
  printf "  'Encounter powers line 1' => q{%s},\n" $ (sixEncounterPowers c) !! 0
  printf "  'Encounter powers line 2' => q{%s},\n" $ (sixEncounterPowers c) !! 1
  printf "  'Encounter powers line 3' => q{%s},\n" $ (sixEncounterPowers c) !! 2
  printf "  'Encounter powers line 4' => q{%s},\n" $ (sixEncounterPowers c) !! 3
  printf "  'Encounter powers line 5' => q{%s},\n" $ (sixEncounterPowers c) !! 4
  printf "  'Encounter powers line 6' => q{%s},\n" $ (sixEncounterPowers c) !! 5
  printf "  'Utility powers line 1' => q{%s},\n"  $ (eightUtilityPowers c) !! 0
  printf "  'Utility powers line 2' => q{%s},\n"  $ (eightUtilityPowers c) !! 1
  printf "  'Utility powers line 3' => q{%s},\n"  $ (eightUtilityPowers c) !! 2
  printf "  'Utility powers line 4' => q{%s},\n"  $ (eightUtilityPowers c) !! 3
  printf "  'Utility powers line 5' => q{%s},\n"  $ (eightUtilityPowers c) !! 4
  printf "  'Utility powers line 6' => q{%s},\n"  $ (eightUtilityPowers c) !! 5
  printf "  'Utility powers line 7' => q{%s},\n"  $ (eightUtilityPowers c) !! 6
  printf "  'Utility powers line 8' => q{%s},\n"  $ (eightUtilityPowers c) !! 7
  printf "  'Magic items line 1' => q{%s},\n" ""
  printf "  'Magic items line 2' => q{%s},\n" ""
  printf "  'Magic items line 3' => q{%s},\n" ""
  printf "  'Magic items line 4' => q{%s},\n" ""
  printf "  'Magic items line 5' => q{%s},\n" ""
  printf "  'Magic items line 6' => q{%s},\n" ""
  printf "  'Magic items line 7' => q{%s},\n" ""
  printf "  'Magic items line 8' => q{%s},\n" ""
  printf "  'Magic items line 9' => q{%s},\n" ""
  printf "  'Magic items line 10' => q{%s},\n" ""
  printf "  'Magic items line 11' => q{%s},\n" ""
  printf "  'Magic items line 12' => q{%s},\n" ""
  printf "  'Magic items line 13' => q{%s},\n" ""
  printf "  'Magic items line 14' => q{%s},\n" ""
  printf "  'Magic items line 15' => q{%s},\n" ""
  printf "  'Magic items line 16' => q{%s},\n" ""
  printf "  'Magic items line 17' => q{%s},\n" ""
  printf "  'Magic items line 18' => q{%s},\n" ""
  printf "  'Magic items line 19' => q{%s},\n" ""
  printf "  'Magic items line 20' => q{%s},\n" ""
  printf "  'Magic items line 21' => q{%s},\n" ""
  printf "  'Magic items line 22' => q{%s},\n" ""
  printf "  'Magic items line 23' => q{%s},\n" ""
  printf "  'Magic items line 24' => q{%s},\n" ""
  printf "  'Magic items line 25' => q{%s},\n" ""
  printf "  'Other equipment line 1' => q{%s},\n" ""
  printf "  'Other equipment line 2' => q{%s},\n" ""
  printf "  'Other equipment line 3' => q{%s},\n" ""
  printf "  'Other equipment line 4' => q{%s},\n" ""
  printf "  'Other equipment line 5' => q{%s},\n" ""
  printf "  'Other equipment line 6' => q{%s},\n" ""
  printf "  'Other equipment line 7' => q{%s},\n" ""
  printf "  'Other equipment line 8' => q{%s},\n" ""
  printf "  'Other equipment line 9' => q{%s},\n" ""
  printf "  'Other equipment line 10' => q{%s},\n" ""
  printf "  'Rituals line 1' => q{%s},\n" ""
  printf "  'Rituals line 2' => q{%s},\n" ""
  printf "  'Rituals line 3' => q{%s},\n" ""
  printf "  'Rituals line 4' => q{%s},\n" ""
  printf "  'Rituals line 5' => q{%s},\n" ""
  printf "  'Rituals line 6' => q{%s},\n" ""
  printf "  'Rituals line 7' => q{%s},\n" ""
  printf "  'Rituals line 8' => q{%s},\n" ""
  printf "  'Rituals line 9' => q{%s},\n" ""
  printf "  'Rituals line 10' => q{%s},\n" ""
  printf "  'Coins and other wealth' => q{%s},\n" ""
  printf "  'Personality traits line 1' => q{%s},\n" ""
  printf "  'Personality traits line 2' => q{%s},\n" ""
  printf "  'Personality traits line 3' => q{%s},\n" ""
  printf "  'Personality traits line 5' => q{%s},\n" ""
  printf "  'Personality traits line 4' => q{%s},\n" ""
  printf "  'Personality traits line 6' => q{%s},\n" ""
  printf "  'Personality traits line 7' => q{%s},\n" ""
  printf "  'Personality traits line 8' => q{%s},\n" ""
  printf "  'Personality traits line 9' => q{%s},\n" ""
  printf "  'Personality traits line 10' => q{%s},\n" ""
  printf "  'Personality traits line 11' => q{%s},\n" ""
  printf "  'Character background line 1' => q{%s},\n" ""
  printf "  'Character background line 2' => q{%s},\n" ""
  printf "  'Character background line 3' => q{%s},\n" ""
  printf "  'Character background line 4' => q{%s},\n" ""
  printf "  'Companions name 1' => q{%s},\n" ""
  printf "  'Companions notes 1' => q{%s},\n" ""
  printf "  'Companions name 2' => q{%s},\n" ""
  printf "  'Companions notes 2' => q{%s},\n" ""
  printf "  'Companions name 3' => q{%s},\n" ""
  printf "  'Companions notes 3' => q{%s},\n" ""
  printf "  'Companions name 4' => q{%s},\n" ""
  printf "  'Companions notes 4' => q{%s},\n" ""
  printf "  'Companions name 5' => q{%s},\n" ""
  printf "  'Companions notes 5' => q{%s},\n" ""
  printf "  'Companions name 6' => q{%s},\n" ""
  printf "  'Companions notes 6' => q{%s},\n" ""
  printf "  'Companions name 7' => q{%s},\n" ""
  printf "  'Companions notes 7' => q{%s},\n" ""
  printf "  'Companions name 8' => q{%s},\n" ""
  printf "  'Companions notes 8' => q{%s},\n" ""
  printf "  'Session and campaign notes 1' => q{%s}\n" ""
  putStrLn "};"

main = do
  printFormData pontus
