module Character where

import qualified Data.List as L
import Data.Maybe
import Modifier
import Taggable
import Race
import Power
import CharacterClass as CC
import Level
import Skill
import Ability
import Equipment
import Equippable
import Feat


data Character = Character { name :: String
                           , playerName :: String
                           , baseStr :: Int
                           , baseDex :: Int
                           , baseCon :: Int
                           , baseInt :: Int
                           , baseWis :: Int
                           , baseCha :: Int
                           , race :: Race
                           , characterClass :: Class
                           , levels :: [Level]
                           , age :: Int
                           , gender :: String
                           , height :: String
                           , weight :: String
                           , alignment :: String
                           , deity :: String
                           , gear :: [Equipment]
                           , xp :: Int
                           , languages :: [String]
                           , adventuringCompanyOrOtherAffiliations :: String
                           , coinAndOtherWealth :: String
                           } deriving (Show)

instance Modifiable Character where
  modifiers c = concat [(Modifier.modifiers . race) c,
                        (CC.modifiers . characterClass) c,
                        (concatMap Feat.modifiers (Character.feats c)),
                        (concatMap Equipment.modifiers (gear c)),
                        (concatMap Level.modifiers (Character.levels c))]

instance Skilled Character where
  skill s c = skillTrainedBonus s c +
              halfLevel c +
              (skillAbilMod s) c +
              skillArmorCheckPenalty s c +
              skillMisc s c
  skillArmorCheckPenalty s c
    | skillArmorCheckPenaltyApplies s (not (wearingLightOrNoArmor c)) = armorCheckPenalty c
    | otherwise = 0
  skillsTrained c = concat [(CC.skillsTrained . characterClass) c
                            -- feats that train in skills
                           ]
  skillTrained s c = s `elem` Skill.skillsTrained c
  skillAbilModPlusHalfLevel s c = halfLevel c + ((skillAbilMod s) c)


{--------------------}
{- Modifier Helpers -}
{--------------------}
characterModsByTarget :: (Modifiable a) => ModTarget -> a -> [Modifier]
characterModsByTarget t c = modsByTarget t $ Modifier.modifiers c

wearingLightOrNoArmor :: Character -> Bool
wearingLightOrNoArmor c = taggedWith (gear c) heavyArmorTag == False


{-------------}
{- Abilities -}
{-------------}
abilityMod Ability.Strength = (strAbilMod)
abilityMod Ability.Dexterity = (dexAbilMod)
abilityMod Ability.Constitution = (conAbilMod)
abilityMod Ability.Intelligence = (intAbilMod)
abilityMod Ability.Wisdom = (wisAbilMod)
abilityMod Ability.Charisma = (chaAbilMod)

str :: Character -> Int
str c = sum $ baseStr c:(map value $ strMods c)

dex :: Character -> Int
dex c = sum $ baseDex c:(map value $ dexMods c)

con :: Character -> Int
con c = sum $ baseCon c:(map value $ conMods c)

int :: Character -> Int
int c = sum $ baseInt c:(map value $ intMods c)

wis :: Character -> Int
wis c = sum $ baseWis c:(map value $ wisMods c)

cha :: Character -> Int
cha c = sum $ baseCha c:(map value $ chaMods c)

abilMod :: Int -> Int
abilMod x = x `div` 2 - 5

wisAbilMod :: Character -> Int
wisAbilMod c = (abilMod . wis) c

dexAbilMod :: Character -> Int
dexAbilMod c = (abilMod . dex) c

conAbilMod :: Character -> Int
conAbilMod c = (abilMod . con) c

intAbilMod :: Character -> Int
intAbilMod c = (abilMod . int) c

strAbilMod :: Character -> Int
strAbilMod c = (abilMod . str) c

chaAbilMod :: Character -> Int
chaAbilMod c = (abilMod . cha) c

abilityMods :: (Modifiable a) => ModTarget -> a -> [Modifier]
abilityMods t c = modsByTarget t $ Modifier.modifiers c

strMods :: (Modifiable a) => a -> [Modifier]
strMods = (abilityMods Modifier.Strength)

dexMods :: (Modifiable a) => a -> [Modifier]
dexMods = (abilityMods Modifier.Dexterity)

conMods :: (Modifiable a) => a -> [Modifier]
conMods = (abilityMods Modifier.Constitution)

intMods :: (Modifiable a) => a -> [Modifier]
intMods = (abilityMods Modifier.Intelligence)

wisMods :: (Modifiable a) => a -> [Modifier]
wisMods = (abilityMods Modifier.Wisdom)

chaMods :: (Modifiable a) => a -> [Modifier]
chaMods = (abilityMods Modifier.Charisma)

strAbilLevel :: Character -> Int
strAbilLevel c = halfLevel c
                 + strAbilMod c

conAbilLevel :: Character -> Int
conAbilLevel c = halfLevel c
                 + conAbilMod c

dexAbilLevel :: Character -> Int
dexAbilLevel c = halfLevel c
                 + dexAbilMod c

intAbilLevel :: Character -> Int
intAbilLevel c = halfLevel c
                 + intAbilMod c

wisAbilLevel :: Character -> Int
wisAbilLevel c = halfLevel c
                 + wisAbilMod c

chaAbilLevel :: Character -> Int
chaAbilLevel c = halfLevel c
                 + chaAbilMod c


{----------}
{- Skills -}
{----------}
skillAbilMod = (abilityMod . skillAbil)

skillTrainedBonus :: SkillName -> Character -> Int
skillTrainedBonus s c
  | trained == True = 5
  | otherwise = 0
  where trained = skillTrained s c

skillArmorPenalty :: Character -> Int
skillArmorPenalty c = sum $ (map value (armorCheckPenaltyMods c))

skillMisc :: SkillName -> Character -> Int
skillMisc s c = modToTarget (skillNameToModTarget s) $ Modifier.modifiers c

miscSkillMods :: SkillName -> [Modifier] -> [Modifier]
miscSkillMods s m = modsToTarget (skillNameToModTarget s) m

acrobatics :: Character -> Int
acrobatics c = skill Skill.Acrobatics c

acrobaticsTrained :: Character -> String
acrobaticsTrained c
  | trained == True = "Yes"
  | otherwise = "Off"
  where trained = skillTrained Skill.Acrobatics c

acrobaticsAbil :: Character -> Int
acrobaticsAbil c = halfLevel c
                   + (skillAbilMod Skill.Acrobatics) c

acrobaticsMisc :: Character -> Int
acrobaticsMisc c = skillMisc Skill.Acrobatics c

arcana :: Character -> Int
arcana c = skill Skill.Arcana c

arcanaTrained :: Character -> String
arcanaTrained c
  | trained == True = "Yes"
  | otherwise = "Off"
  where trained = skillTrained Skill.Arcana c

arcanaAbil :: Character -> Int
arcanaAbil c = halfLevel c
               + (skillAbilMod Skill.Arcana) c

arcanaMisc :: Character -> Int
arcanaMisc c = skillMisc Skill.Arcana c

athletics :: Character -> Int
athletics c = skill Skill.Athletics c

athleticsTrained :: Character -> String
athleticsTrained c
  | trained == True = "Yes"
  | otherwise = "Off"
  where trained = skillTrained Skill.Athletics c

athleticsAbil :: Character -> Int
athleticsAbil c = halfLevel c
                  + (skillAbilMod Skill.Athletics) c

athleticsMisc :: Character -> Int
athleticsMisc c = skillMisc Skill.Athletics c

bluff :: Character -> Int
bluff c = skill Skill.Bluff c

bluffTrained :: Character -> String
bluffTrained c
  | trained == True = "Yes"
  | otherwise = "Off"
  where trained = skillTrained Skill.Bluff c

bluffAbil :: Character -> Int
bluffAbil c = halfLevel c
              + (skillAbilMod Skill.Bluff) c

bluffMisc :: Character -> Int
bluffMisc c = skillMisc Skill.Bluff c

diplomacy :: Character -> Int
diplomacy c = skill Skill.Diplomacy c

diplomacyTrained :: Character -> String
diplomacyTrained c
  | trained == True = "Yes"
  | otherwise = "Off"
  where trained = skillTrained Skill.Diplomacy c

diplomacyAbil :: Character -> Int
diplomacyAbil c = halfLevel c
                  + (skillAbilMod Skill.Diplomacy) c

diplomacyMisc :: Character -> Int
diplomacyMisc c = skillMisc Skill.Diplomacy c

dungeoneering :: Character -> Int
dungeoneering c = skill Skill.Dungeoneering c

dungeoneeringTrained :: Character -> String
dungeoneeringTrained c
  | trained == True = "Yes"
  | otherwise = "Off"
  where trained = skillTrained Skill.Dungeoneering c

dungeoneeringAbil :: Character -> Int
dungeoneeringAbil c = halfLevel c
                      + (skillAbilMod Skill.Dungeoneering) c

dungeoneeringMisc :: Character -> Int
dungeoneeringMisc c = skillMisc Skill.Dungeoneering c

endurance :: Character -> Int
endurance c = skill Skill.Endurance c

enduranceTrained :: Character -> String
enduranceTrained c
  | trained == True = "Yes"
  | otherwise = "Off"
  where trained = skillTrained Skill.Endurance c

enduranceAbil :: Character -> Int
enduranceAbil c = halfLevel c
                  + (skillAbilMod Skill.Endurance) c

enduranceMisc :: Character -> Int
enduranceMisc c = skillMisc Skill.Endurance c

heal :: Character -> Int
heal c = skill Skill.Heal c

healTrained :: Character -> String
healTrained c
  | trained == True = "Yes"
  | otherwise = "Off"
  where trained = skillTrained Skill.Heal c

healAbil :: Character -> Int
healAbil c = halfLevel c
             + (skillAbilMod Skill.Heal) c

healMisc :: Character -> Int
healMisc c = skillMisc Skill.Heal c

history :: Character -> Int
history c = skill Skill.History c

historyTrained :: Character -> String
historyTrained c
  | trained == True = "Yes"
  | otherwise = "Off"
  where trained = skillTrained Skill.History c

historyAbil :: Character -> Int
historyAbil c = halfLevel c
                + (skillAbilMod Skill.History) c

historyMisc :: Character -> Int
historyMisc c = skillMisc Skill.History c

insight :: Character -> Int
insight c = skill Skill.Insight c

insightTrained :: Character -> String
insightTrained c
  | trained == True = "Yes"
  | otherwise = "Off"
  where trained = skillTrained Skill.Insight c

insightAbil :: Character -> Int
insightAbil c = halfLevel c
                + (skillAbilMod Skill.Insight) c

insightMisc :: Character -> Int
insightMisc c = skillMisc Skill.Insight c

intimidate :: Character -> Int
intimidate c = skill Skill.Intimidate c

intimidateTrained :: Character -> String
intimidateTrained c
  | trained == True = "Yes"
  | otherwise = "Off"
  where trained = skillTrained Skill.Intimidate c

intimidateAbil :: Character -> Int
intimidateAbil c = halfLevel c
                   + (skillAbilMod Skill.Intimidate) c

intimidateMisc :: Character -> Int
intimidateMisc c = skillMisc Skill.Intimidate c

nature :: Character -> Int
nature c = skill Skill.Nature c

natureTrained :: Character -> String
natureTrained c
  | trained == True = "Yes"
  | otherwise = "Off"
  where trained = skillTrained Skill.Nature c

natureAbil :: Character -> Int
natureAbil c = halfLevel c
               + (skillAbilMod Skill.Nature) c

natureMisc :: Character -> Int
natureMisc c = skillMisc Skill.Nature c

perception :: Character -> Int
perception c = skill Skill.Perception c

perceptionTrained :: Character -> String
perceptionTrained c
  | trained == True = "Yes"
  | otherwise = "Off"
  where trained = skillTrained Skill.Perception c

perceptionAbil :: Character -> Int
perceptionAbil c = halfLevel c
                   + (skillAbilMod Skill.Perception) c

perceptionMisc :: Character -> Int
perceptionMisc c = skillMisc Skill.Perception c

religion :: Character -> Int
religion c = skill Skill.Religion c

religionTrained :: Character -> String
religionTrained c
  | trained == True = "Yes"
  | otherwise = "Off"
  where trained = skillTrained Skill.Religion c

religionAbil :: Character -> Int
religionAbil c = halfLevel c
                 + (skillAbilMod Skill.Religion) c

religionMisc :: Character -> Int
religionMisc c = skillMisc Skill.Religion c

stealth :: Character -> Int
stealth c = skill Skill.Stealth c

stealthTrained :: Character -> String
stealthTrained c
  | trained == True = "Yes"
  | otherwise = "Off"
  where trained = skillTrained Skill.Stealth c

stealthAbil :: Character -> Int
stealthAbil c = halfLevel c
                + (skillAbilMod Skill.Stealth) c

stealthMisc :: Character -> Int
stealthMisc c = skillMisc Skill.Stealth c

streetwise :: Character -> Int
streetwise c = skill Skill.Streetwise c

streetwiseTrained :: Character -> String
streetwiseTrained c
  | trained == True = "Yes"
  | otherwise = "Off"
  where trained = skillTrained Skill.Streetwise c

streetwiseAbil :: Character -> Int
streetwiseAbil c = halfLevel c
                   + (skillAbilMod Skill.Streetwise) c

streetwiseMisc :: Character -> Int
streetwiseMisc c = skillMisc Skill.Streetwise c

thievery :: Character -> Int
thievery c = skill Skill.Thievery c

thieveryTrained :: Character -> String
thieveryTrained c
  | trained == True = "Yes"
  | otherwise = "Off"
  where trained = skillTrained Skill.Thievery c

thieveryAbil :: Character -> Int
thieveryAbil c = halfLevel c
                 + (skillAbilMod Skill.Thievery) c

thieveryMisc :: Character -> Int
thieveryMisc c = skillMisc Skill.Thievery c


{------------}
{- Defenses -}
{------------}
{- Fortitude -}
fort :: Character -> Int
fort c = fortAbil c
         + tenPlusHalfLevel c
         + (modToTarget Fortitude $ Modifier.modifiers c)

fortAbil :: Character -> Int
fortAbil c = maximum [strAbilMod c, conAbilMod c]

fortClass :: Character -> Int
fortClass c = Modifier.mod Fortitude ClassMod c

fortEnh :: Character -> Int
fortEnh c = Modifier.mod Fortitude EnhancementMod c

fortFeat :: Character -> Int
fortFeat c = Modifier.mod Fortitude FeatMod c

fortMiscMods :: Character -> [Modifier]
fortMiscMods c = filter (\m -> modType m `notElem` specificTypes) $ fortMods c
  where specificTypes = [ClassMod, EnhancementMod, FeatMod]

fortMods :: (Modifiable a) => a -> [Modifier]
fortMods = (characterModsByTarget Fortitude)

fortMisc1 :: Character -> Int
fortMisc1 c
  | length mods > 0 = value $ last $ sortByValue mods
  | otherwise = 0
  where mods = fortMiscMods c

fortMisc2 :: Character -> Int
fortMisc2 c
  | length mods > 1 = value $ last $ L.init $ sortByValue mods
  | otherwise = 0
  where mods = fortMiscMods c

{- Reflex -}
ref :: Character -> Int
ref c = refAbil c
        + tenPlusHalfLevel c
        + (modToTarget Reflex $ Modifier.modifiers c)

refAbil :: Character -> Int
refAbil c = maximum [dexAbilMod c, intAbilMod c]

refClass :: Character -> Int
refClass c = Modifier.mod Reflex ClassMod c

refEnh :: Character -> Int
refEnh c = Modifier.mod Reflex EnhancementMod c

refFeat :: Character -> Int
refFeat c = Modifier.mod Reflex FeatMod c

refMiscMods :: Character -> [Modifier]
refMiscMods c = filter (\m -> modType m `notElem` specificTypes) $ refMods c
  where specificTypes = [ClassMod, EnhancementMod, FeatMod]

refMods :: (Modifiable a) => a -> [Modifier]
refMods = (characterModsByTarget Reflex)

refMisc1 :: Character -> Int
refMisc1 c
  | length mods > 0 = value $ last $ sortByValue mods
  | otherwise = 0
  where mods = refMiscMods c

refMisc2 :: Character -> Int
refMisc2 c
  | length mods > 1 = value $ last $ L.init $ sortByValue mods
  | otherwise = 0
  where mods = refMiscMods c

{- Will -}
will :: Character -> Int
will c = willAbil c
         + tenPlusHalfLevel c
         + (modToTarget Will $ Modifier.modifiers c)

willAbil :: Character -> Int
willAbil c = maximum [wisAbilMod c, chaAbilMod c]

willClass :: Character -> Int
willClass c = Modifier.mod Will ClassMod c

willEnh :: Character -> Int
willEnh c = Modifier.mod Will EnhancementMod c

willFeat :: Character -> Int
willFeat c = Modifier.mod Will FeatMod c

willMiscMods :: Character -> [Modifier]
willMiscMods c = filter (\m -> modType m `notElem` specificTypes) $ willMods c
  where specificTypes = [ClassMod, EnhancementMod, FeatMod]

willMods :: (Modifiable a) => a -> [Modifier]
willMods = (characterModsByTarget Will)

willMisc1 :: Character -> Int
willMisc1 c
  | length mods > 0 = value $ last $ sortByValue mods
  | otherwise = 0
  where mods = willMiscMods c

willMisc2 :: Character -> Int
willMisc2 c
  | length mods > 1 = value $ last $ L.init $ sortByValue mods
  | otherwise = 0
  where mods = willMiscMods c

{- Armor Class -}
ac :: Character -> Int
ac c = tenPlusHalfLevel c
       + acAbil c
       + (modToTarget ArmorClass $ Modifier.modifiers c)

acArmorAbility :: Character -> Int
acArmorAbility c = acAbil c + Modifier.mod ArmorClass ArmorMod c

acAbil :: Character -> Int
acAbil c
  | wearingLightOrNoArmor c == True = maximum [intAbilMod c, dexAbilMod c]
  | otherwise = 0

acClass :: Character -> Int
acClass c = Modifier.mod ArmorClass ClassMod c

acEnh :: Character -> Int
acEnh c = Modifier.mod ArmorClass EnhancementMod c

acFeat :: Character -> Int
acFeat c = Modifier.mod ArmorClass FeatMod c

acMiscMods :: Character -> [Modifier]
acMiscMods c = filter (\m -> modType m `notElem` specificTypes) $ acMods c
  where specificTypes = [ArmorMod, ClassMod, EnhancementMod, FeatMod]

acMods :: (Modifiable a) => a -> [Modifier]
acMods = (characterModsByTarget ArmorClass)

acMisc1 :: Character -> Int
acMisc1 c
  | length mods > 0 = value $ last $ sortByValue mods
  | otherwise = 0
  where mods = acMiscMods c

acMisc2 :: Character -> Int
acMisc2 c
  | length mods > 1 = value $ last $ L.init $ sortByValue mods
  | otherwise = 0
  where mods = acMiscMods c


{---------}
{- Speed -}
{---------}
speed c = Character.speedBase c
          + (modToTarget Speed $ Modifier.modifiers c)

speedBase :: Character -> Int
speedBase = (Race.baseSpeed .Character.race)

speedArmor :: Character -> Int
speedArmor c = Modifier.mod Speed ArmorMod  c

speedItem :: Character -> Int
speedItem c = Modifier.mod Speed ItemMod c

miscModToSpeed :: Character -> Int
miscModToSpeed c
  | length mods > 0 = value $ last $ sortByValue mods
  | otherwise = 0
  where mods = miscModsToSpeed c

miscModsToSpeed :: Character -> [Modifier]
miscModsToSpeed c = filter (specificFilter) $ speedMods c
  where specificTypes = [ArmorMod, ItemMod]
        specificFilter = (\m -> modType m `notElem` specificTypes)

speedMods :: (Modifiable a) => a -> [Modifier]
speedMods = (characterModsByTarget Speed)


{---------}
{- Level -}
{---------}
level :: Character -> Int
level c = length $ levels c

halfLevel :: Character -> Int
halfLevel c = Character.level c `div` 2

tenPlusHalfLevel :: Character -> Int
tenPlusHalfLevel c = 10 + halfLevel c


{--------------}
{- Initiative -}
{--------------}
-- is initiative affected by the armor penalty in 4E? NO.
-- add other mods, feats, powers, equipment
init :: Character -> Int
init c = maximum [(intAbilMod c), (dexAbilMod c)] + (halfLevel c)

-- initiative mods called out: 1/2 level, dex mod, and misc. Since this is
-- less detailed than many other fields, I am going to sum all valid mods and
-- use that for the misc field.
initMisc c = modToTarget Initiative $ Modifier.modifiers c


{---------}
{- Feats -}
{---------}
feat :: Int -> Character -> String
feat i c
  | length feats > i = feats !! i
  | otherwise = ""
  where
    feats = map Feat.name $ L.sort $ Character.feats c

feats :: Character -> [Feat]
feats c = concatMap Level.feats $ levels c


{--------------}
{- Hit Points -}
{--------------}
hp :: Character -> Int
hp c = con c
       + (hpAtFirstLevel . characterClass) c
       + ((hpPerLevelGained . characterClass) c * (Character.level c - 1))
       + (modToTarget HitPoints $ Modifier.modifiers c)

hpMods c = modsToTarget HitPoints $ Modifier.modifiers c

bloodied :: Character -> Int
bloodied c = hp c `div` 2

surgeValue :: Character -> Int
surgeValue c = hp c `div` 4

surgesDay :: Character -> Int
surgesDay c = conAbilMod c
              + (CC.surgesDay . characterClass) c


{----------}
{- Powers -}
{----------}
powers :: Character -> [Power]
-- powers c = powersFromLevels c :: powersFromItems c
powers c = concat [concatMap Level.powers (levels c), -- TODO racial & class
                   concatMap Equipment.powers (gear c)]

powersByType :: PowerType -> [Power] -> [Power]
powersByType t p = filter (\p -> Power.powerType p == t) p

attackPowers :: [Power] -> [Power]
attackPowers p = filter (\p -> Power.powerType p == Power.Attack) p

racialFeature :: Int -> Character -> String
racialFeature i c
  | length powers > i = powers !! i
  | otherwise = ""
  where
    powers = map Power.name $ L.sort $ racialFeatures $ Character.powers c

racialFeatures :: [Power] -> [Power]
racialFeatures p = filter (\p -> Power.powerType p == RacialFeature) p

classFeature :: Int -> Character -> String
classFeature i c
  | length powers > i = powers !! i
  | otherwise = ""
  where
    powers = map Power.name $ L.sort $ classFeatures $ Character.powers c

classFeatures :: [Power] -> [Power]
classFeatures p = filter (\p -> Power.powerType p == ClassFeature) p

pathFeature :: Int -> Character -> String
pathFeature i c
  | length powers > i = powers !! i
  | otherwise = ""
  where
    powers = map Power.name $ L.sort $ pathFeatures $ Character.powers c

pathFeatures :: [Power] -> [Power]
pathFeatures p = filter (\p -> Power.powerType p == PathFeature) p

destinyFeature :: Int -> Character -> String
destinyFeature i c
  | length powers > i = powers !! i
  | otherwise = ""
  where
    powers = map Power.name $ L.sort $ destinyFeatures $ Character.powers c

destinyFeatures :: [Power] -> [Power]
destinyFeatures p = filter (\p -> Power.powerType p == DestinyFeature) p

classPathOrDestinyFeature :: Int -> Character -> String
classPathOrDestinyFeature i c
  | length powers > i = powers !! i
  | otherwise = ""
  where
    powers = map Power.name $ L.sort $ classPathOrDestinyFeatures $ Character.powers c

classPathOrDestinyFeatures :: [Power] -> [Power]
classPathOrDestinyFeatures p = destinyFeatures p
                               ++ pathFeatures p
                               ++ classFeatures p

utilityPower :: Int -> Character -> String
utilityPower i c
  | length powers > i = powers !! i
  | otherwise = ""
  where
    powers = map Power.name $ L.sort $ utilityPowers $ Character.powers c

utilityPowers :: [Power] -> [Power]
utilityPowers p = powersByType Utility p

nonUtilityPowers :: [Power] -> [Power]
nonUtilityPowers p = filter (\x -> x `notElem` utils) p
  where utils = utilityPowers p

powersByUses :: PowerUses -> [Power] -> [Power]
powersByUses u p = filter (\p -> Power.uses p == u) p

atWillPower :: Int -> Character -> String
atWillPower i c
  | length powers > i = powers !! i
  | otherwise = ""
  where
    powers = map Power.name $ L.sort $ atWillPowers $ Character.powers c

atWillPowers :: [Power] -> [Power]
atWillPowers p = powersByUses AtWill p

encounterPower :: Int -> Character -> String
encounterPower i c
  | length powers > i = powers !! i
  | otherwise = ""
  where
    powers = map Power.name $ L.sort $ (nonUtilityPowers . encounterPowers) $ Character.powers c

encounterPowers :: [Power] -> [Power]
encounterPowers p = powersByUses Encounter p

dailyPower :: Int -> Character -> String
dailyPower i c
  | length powers > i = powers !! i
  | otherwise = ""
  where
    powers = map Power.name $ L.sort $ dailyPowers $ Character.powers c

dailyPowers :: [Power] -> [Power]
dailyPowers p = powersByUses Daily p


{-------------}
{- Languages -}
{-------------}
language :: Int -> Character -> String
language i c
  | length langs > i = langs !! i
  | otherwise = ""
  where
    langs = languages c


{-----------}
{- Attacks -}
{-----------}
{- Overkill? Macro? -}
attackBonus :: Power -> Equipment -> Character -> Int
attackBonus p e c = halfLevel c
                    + attackAbil p c
                    + attackClass p c
                    + attackProf p e c
                    + attackFeat p c
                    + attackEnh p e c
                    + attackMisc p c

attackAbil :: Power -> Character -> Int
attackAbil p c
  | Power.attackAbility p == Nothing = 0
  | otherwise = (abilityMod $ (fromJust (Power.attackAbility p))) c

attackClass :: Power -> Character -> Int
attackClass p c = 0 -- TODO

attackProf :: Power -> Equipment -> Character -> Int
attackProf p e c = powerProf p e c

attackFeat :: Power -> Character -> Int
attackFeat p c = 0 -- TODO

attackEnh :: Power -> Equipment -> Character -> Int
attackEnh p e c = Modifier.mod Modifier.Attack EnhancementMod e

attackMisc :: Power -> Character -> Int
attackMisc p c = 0 -- TODO

attack1Name :: Character -> String
attack1Name c = Power.name $ attack1Power c

attack1Power :: Character -> Power
attack1Power c = head $ atWillPowers $ attackPowers $ Character.powers c

attack1Bonus :: Character -> Int
attack1Bonus c = attackBonus (attack1Power c) (primaryWeapon c) c

attack1Abil :: Character -> Int
attack1Abil c = attackAbil (attack1Power c) c

attack1Class :: Character -> Int
attack1Class c = attackClass (attack1Power c) c

attack1Prof :: Character -> Int
attack1Prof c = attackProf (attack1Power c) (primaryWeapon c) c

attack1Feat :: Character -> Int
attack1Feat c = attackFeat (attack1Power c) c

attack1Enh :: Character -> Int
attack1Enh c = attackEnh (attack1Power c) (primaryWeapon c) c

attack1Misc :: Character -> Int
attack1Misc c = attackMisc (attack1Power c) c

attack2Name :: Character -> String
attack2Name c = Power.name $ attack2Power c

attack2Power :: Character -> Power
attack2Power c = head $ tail $ atWillPowers $ attackPowers $ Character.powers c

attack2Bonus :: Character -> Int
attack2Bonus c = attackBonus (attack2Power c) (primaryWeapon c) c

attack2Abil :: Character -> Int
attack2Abil c = attackAbil (attack2Power c) c

attack2Class :: Character -> Int
attack2Class c = attackClass (attack2Power c) c

attack2Prof :: Character -> Int
attack2Prof c = attackProf (attack2Power c) (primaryWeapon c) c

attack2Feat :: Character -> Int
attack2Feat c = attackFeat (attack2Power c) c

attack2Enh :: Character -> Int
attack2Enh c = attackEnh (attack2Power c) (primaryWeapon c) c

attack2Misc :: Character -> Int
attack2Misc c = attackMisc (attack2Power c) c


{----------}
{- Damage -}
{----------}
{- Overkill? -}
damageMod :: Power -> Character -> Int
damageMod p c = damageAbil p c
                + damageFeat p c
                + damageEnh p c
                + damageMisc p c

damageDesc :: Power -> Character -> String
damageDesc p c
  | Power.damage p == Nothing = ""
  | otherwise = (fromJust $ Power.damage p) ++ "+" ++ (show $ damageMod p c)

damageAbil :: Power -> Character -> Int
damageAbil p c = attackAbil p c

damageFeat :: Power -> Character -> Int
damageFeat p c = 0 -- TODO

damageEnh :: Power -> Character -> Int
damageEnh p c = Modifier.mod Modifier.Damage EnhancementMod c

damageMisc :: Power -> Character -> Int
damageMisc p c = 0 -- TODO

damageMisc1 :: Power -> Character -> Int
damageMisc1 p c = 0 -- TODO

damageMisc2 :: Power -> Character -> Int
damageMisc2 p c = 0 -- TODO

damage1Desc :: Character -> String
damage1Desc c = damageDesc (attack1Power c) c

damage1Mod :: Character -> Int
damage1Mod c = damageMod (attack1Power c) c

damage1Abil :: Character -> Int
damage1Abil c = damageAbil (attack1Power c) c

damage1Feat :: Character -> Int
damage1Feat c = damageFeat (attack1Power c) c

damage1Enh :: Character -> Int
damage1Enh c = damageEnh (attack1Power c) c

damage1Misc1 :: Character -> Int
damage1Misc1 c = damageMisc1 (attack1Power c) c

damage1Misc2 :: Character -> Int
damage1Misc2 c = damageMisc2 (attack1Power c) c

damage2Desc :: Character -> String
damage2Desc c = damageDesc (attack1Power c) c

damage2Mod :: Character -> Int
damage2Mod c = damageMod (attack1Power c) c

damage2Abil :: Character -> Int
damage2Abil c = damageAbil (attack1Power c) c

damage2Feat :: Character -> Int
damage2Feat c = damageFeat (attack1Power c) c

damage2Enh :: Character -> Int
damage2Enh c = damageEnh (attack1Power c) c

damage2Misc1 :: Character -> Int
damage2Misc1 c = damageMisc1 (attack1Power c) c

damage2Misc2 :: Character -> Int
damage2Misc2 c = damageMisc2 (attack1Power c) c


{----------------------}
{- Looking for a home -}
{----------------------}
basicMeleePower = Power { Power.name="Melee Basic Attack"
                        , Power.attackAbility=Just Ability.Strength
                        , Power.attackVsDefense=Just "AC"
                        , Power.uses=AtWill
                        , Power.keywords=["Weapon"]
                        , Power.level=1
                        , Power.action=StandardAction
                        , Power.powerType=Power.Attack
                        , Power.damage=Just "1[W]"
                        }
basicRangedPower = Power { Power.name="Ranged Basic Attack"
                         , Power.attackAbility=Just Ability.Dexterity
                         , Power.attackVsDefense=Just "AC"
                         , Power.uses=AtWill
                         , Power.keywords=["Weapon"]
                         , Power.level=1
                         , Power.action=StandardAction
                         , Power.powerType=Power.Attack
                         , Power.damage=Just "1[W]"
                         }

size :: Character -> String
size = (Race.size . Character.race)

raceName :: Character -> String
raceName = (Race.name . Character.race)

racialAbilModifiers :: Character -> String
racialAbilModifiers c = Race.abilModifiers $ Character.race c

basic1MeleeAttack :: Character -> Int
basic1MeleeAttack c = attackBonus basicMeleePower (primaryWeapon c) c

basic1MeleeDamage :: Character -> String
basic1MeleeDamage c = basicMeleeDamage c $ primaryWeapon c

basic1MeleeDefense :: Character -> String
basic1MeleeDefense c = basicDefense basicMeleePower c

basic1MeleeWeaponOrPower :: Character -> String
basic1MeleeWeaponOrPower c = Equipment.name $ primaryWeapon c

basic2RangedAttack :: Character -> Int
basic2RangedAttack c = attackBonus basicRangedPower (secondaryWeapon c) c

basic2RangedDamage :: Character -> String
basic2RangedDamage c = basicRangedDamage c $ secondaryWeapon c

basic2RangedDefense :: Character -> String
basic2RangedDefense c = "AC"

basic2RangedWeaponOrPower :: Character -> String
basic2RangedWeaponOrPower c = Equipment.name $ secondaryWeapon c


basicMeleeAttack c w = basicAttack c w + strAbilMod c

basicMeleeDamage :: Character -> Equipment -> String
basicMeleeDamage c w = "1[W]+" ++ (show $ mods)
  where mods = sum [strAbilMod c, damageEnh basicMeleePower c]

basicRangedDamage :: Character -> Equipment -> String
basicRangedDamage c w = "1[W]+" ++ (show $ strAbilMod c)

basicDefense :: Power -> Character -> String
basicDefense p c = fromJust $ Power.attackVsDefense p

basicAttack c w = halfLevel c + (weaponProficiencyBonus c w)

basicRangedAttack c w = basicAttack c w + dexAbilMod c

armorCheckPenalty c = sum $ (map value (armorCheckPenaltyMods c))

armorCheckPenaltyMods c = filter (\mod -> Modifier.target mod == Modifier.Skill) (Modifier.modifiers c)

weaponProficiencyBonus c w
  | isProficientWith c w == True = fromJust $ Equipment.proficiencyBonus w
  | otherwise = 0

primaryWeapon c = head $ (weapons . gear) c

secondaryWeapon c = head $ tail $ (weapons . gear) c

weapons :: [Equipment] -> [Equipment]
weapons w = filter (hasTag (tagFactory "Weapon")) w

escapeParen :: String -> String
escapeParen s = concatMap (\x -> (ep x)) s
  where ep = (\x -> if x `elem` "()" then "\\" ++ [x] else [x])

magicItem :: Int -> Character -> String
magicItem i c
  | length eligItems > i = eligItems !! i
  | otherwise = ""
  where eligItems = filter (\x -> x `notElem` slotItems) items
        slotItems = map (\x -> magicItemInSlot (0 :: Int) x c) magicItemSlots
        items = map (escapeParen . Equipment.name) $ magicItems $ Character.gear c

magicItemSlots :: [String]
magicItemSlots = ["Armor",
                  "Arms",
                  "Feet",
                  "Hands",
                  "Head",
                  "Neck",
                  "Ring",
                  "Waist",
                  "Weapon"
                 ]

magicItemInSlot :: Int -> String -> Character -> String
magicItemInSlot x s c
  | length items > x = (escapeParen . Equipment.name) $ items !! x
  | otherwise = ""
  where items = gearInSlot s $ magicItems $ Character.gear c

magicItems :: [Equipment] -> [Equipment]
magicItems e = filter (hasTag (tagFactory "Magic")) e

gearInSlot :: String -> [Equipment] -> [Equipment]
gearInSlot s g = filter (hasTag (tagFactory s)) g

isArmed c
  | length ((weapons . gear) c) == 0 = False
  | otherwise = True

isProficientWith c w = grantsProficiencyWith (characterClass c) w -- TODO feats

powerProf :: Power -> Equipment -> Character -> Int
powerProf p e c
  | powerHasKeyword p "Weapon" == True = prof
  | otherwise = 0
  where prof = fromJust $ proficiencyBonus e

classModifierFor c p =
  maximum $ 0:(map value $ filter (\mod -> (show $ Modifier.target mod) == Power.name p) $ CC.modifiers $ characterClass c)

featModifierFor c p =
  maximum $ 0:(map value $ filter (\mod -> (show $ Modifier.target mod) == Power.name p) $ concatMap Feat.modifiers $ Character.feats c)

className c = (CC.name . characterClass) c

savingThrowMods :: Character -> String
savingThrowMods c = unwords' $ map Modifier.name mods
  where mods = modsByTarget SavingThrow $ Modifier.modifiers c

unwords' :: [String] -> String
unwords' ws = foldr1 (\w s -> w ++ ", " ++ s) ws

resistances :: Character -> String
resistances c = unwords' $ map Modifier.name mods
  where mods = modsByTarget Resistance $ Modifier.modifiers c

otherEquipment :: Int -> Character -> String
otherEquipment i c
  | length items > i = (escapeParen . Equipment.name) $ items !! i
  | otherwise = ""
  where items = L.sort $ filter (\x -> x `notElem` mi) $ gear c
        mi = magicItems $ gear c
