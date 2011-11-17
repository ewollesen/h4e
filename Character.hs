module Character where

import Data.List
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
              miscModToSkill s c
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
skillArmorPenalty c = (-1)

miscModToSkill :: SkillName -> Character -> Int
miscModToSkill s c = modToTarget (skillNameToModTarget s) $ Modifier.modifiers c

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


{------------}
{- Defenses -}
{------------}
{- Fortitude -}
fortitude :: Character -> Int
fortitude c = abilModToFort c
              + tenPlusHalfLevel c
              + (modToTarget Fortitude $ Modifier.modifiers c)

abilModToFort :: Character -> Int
abilModToFort c = maximum [strAbilMod c, conAbilMod c]

classModToFort :: Character -> Int
classModToFort c = Modifier.mod Fortitude ClassMod c

enhModToFort :: Character -> Int
enhModToFort c = Modifier.mod Fortitude EnhancementMod c

featModToFort :: Character -> Int
featModToFort c = Modifier.mod Fortitude FeatMod c

miscModsToFort :: Character -> [Modifier]
miscModsToFort c = filter (\m -> modType m `notElem` specificTypes) $ fortMods c
  where specificTypes = [ClassMod, EnhancementMod, FeatMod]

fortMods :: (Modifiable a) => a -> [Modifier]
fortMods = (characterModsByTarget Fortitude)

misc1ModToFort :: Character -> Int
misc1ModToFort c
  | length mods > 0 = value $ last $ sortByValue mods
  | otherwise = 0
  where mods = miscModsToFort c

misc2ModToFort :: Character -> Int
misc2ModToFort c
  | length mods > 1 = value $ last $ init $ sortByValue mods
  | otherwise = 0
  where mods = miscModsToFort c

{- Reflex -}
reflex :: Character -> Int
reflex c = abilModToRef c
         + tenPlusHalfLevel c
         + (modToTarget Reflex $ Modifier.modifiers c)

abilModToRef :: Character -> Int
abilModToRef c = maximum [dexAbilMod c, intAbilMod c]

classModToRef :: Character -> Int
classModToRef c = Modifier.mod Reflex ClassMod c

enhModToRef :: Character -> Int
enhModToRef c = Modifier.mod Reflex EnhancementMod c

featModToRef :: Character -> Int
featModToRef c = Modifier.mod Reflex FeatMod c

miscModsToRef :: Character -> [Modifier]
miscModsToRef c = filter (\m -> modType m `notElem` specificTypes) $ refMods c
  where specificTypes = [ClassMod, EnhancementMod, FeatMod]

refMods :: (Modifiable a) => a -> [Modifier]
refMods = (characterModsByTarget Reflex)

misc1ModToRef :: Character -> Int
misc1ModToRef c
  | length mods > 0 = value $ last $ sortByValue mods
  | otherwise = 0
  where mods = miscModsToRef c

misc2ModToRef :: Character -> Int
misc2ModToRef c
  | length mods > 1 = value $ last $ init $ sortByValue mods
  | otherwise = 0
  where mods = miscModsToRef c

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
  | length mods > 1 = value $ last $ init $ sortByValue mods
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
  | length mods > 1 = value $ last $ init $ sortByValue mods
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
initiative :: Character -> Int
initiative c = maximum [(intAbilMod c), (dexAbilMod c)] + (halfLevel c)

-- initiative mods called out: 1/2 level, dex mod, and misc. Since this is
-- less detailed than many other fields, I am going to sum all valid mods and
-- use that for the misc field.
miscModToInit c = modToTarget Initiative $ Modifier.modifiers c


{---------}
{- Feats -}
{---------}
feat :: Int -> Character -> String
feat i c
  | length feats > i = feats !! i
  | otherwise = ""
  where
    feats = map Feat.name $ sort $ Character.feats c

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
powers c = concatMap Level.powers (levels c) -- TODO racial & class

powersByType :: PowerType -> [Power] -> [Power]
powersByType t p = filter (\p -> Power.powerType p == t) p

attackPowers :: [Power] -> [Power]
attackPowers p = filter (\p -> Power.powerType p == Power.Attack) p

racialFeature :: Int -> Character -> String
racialFeature i c
  | length powers > i = powers !! i
  | otherwise = ""
  where
    powers = map Power.name $ sort $ racialFeatures $ Character.powers c

racialFeatures :: [Power] -> [Power]
racialFeatures p = filter (\p -> Power.powerType p == RacialFeature) p

classFeature :: Int -> Character -> String
classFeature i c
  | length powers > i = powers !! i
  | otherwise = ""
  where
    powers = map Power.name $ sort $ classFeatures $ Character.powers c

classFeatures :: [Power] -> [Power]
classFeatures p = filter (\p -> Power.powerType p == ClassFeature) p

pathFeature :: Int -> Character -> String
pathFeature i c
  | length powers > i = powers !! i
  | otherwise = ""
  where
    powers = map Power.name $ sort $ pathFeatures $ Character.powers c

pathFeatures :: [Power] -> [Power]
pathFeatures p = filter (\p -> Power.powerType p == PathFeature) p

destinyFeature :: Int -> Character -> String
destinyFeature i c
  | length powers > i = powers !! i
  | otherwise = ""
  where
    powers = map Power.name $ sort $ destinyFeatures $ Character.powers c

destinyFeatures :: [Power] -> [Power]
destinyFeatures p = filter (\p -> Power.powerType p == DestinyFeature) p

classPathOrDestinyFeature :: Int -> Character -> String
classPathOrDestinyFeature i c
  | length powers > i = powers !! i
  | otherwise = ""
  where
    powers = map Power.name $ sort $ classPathOrDestinyFeatures $ Character.powers c

classPathOrDestinyFeatures :: [Power] -> [Power]
classPathOrDestinyFeatures p = destinyFeatures p
                               ++ pathFeatures p
                               ++ classFeatures p

utilityPower :: Int -> Character -> String
utilityPower i c
  | length powers > i = powers !! i
  | otherwise = ""
  where
    powers = map Power.name $ sort $ utilityPowers $ Character.powers c

utilityPowers :: [Power] -> [Power]
utilityPowers p = powersByType Utility p

powersByUses :: PowerUses -> [Power] -> [Power]
powersByUses u p = filter (\p -> Power.uses p == u) p

atWillPower :: Int -> Character -> String
atWillPower i c
  | length powers > i = powers !! i
  | otherwise = ""
  where
    powers = map Power.name $ sort $ atWillPowers $ Character.powers c

atWillPowers :: [Power] -> [Power]
atWillPowers p = powersByUses AtWill p

encounterPower :: Int -> Character -> String
encounterPower i c
  | length powers > i = powers !! i
  | otherwise = ""
  where
    powers = map Power.name $ sort $ encounterPowers $ Character.powers c

encounterPowers :: [Power] -> [Power]
encounterPowers p = powersByUses Encounter p

dailyPower :: Int -> Character -> String
dailyPower i c
  | length powers > i = powers !! i
  | otherwise = ""
  where
    powers = map Power.name $ sort $ dailyPowers $ Character.powers c

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
{- Overkill? -}
attackMod :: Power -> Character -> Int
attackMod p c = halfLevel c
                + attackAbilMod p c
                + attackClassMod p c
                + attackProfMod p c
                + attackFeatMod p c
                + attackEnhMod p c
                + attackMiscMod p c

attackAbilMod :: Power -> Character -> Int
attackAbilMod p c
  | Power.attackAbility p == Nothing = 0
  | otherwise = (abilityMod $ (fromJust (Power.attackAbility p))) c

attackClassMod :: Power -> Character -> Int
attackClassMod p c = 0 -- TODO

attackProfMod :: Power -> Character -> Int
attackProfMod = (profModToPower)

attackFeatMod :: Power -> Character -> Int
attackFeatMod p c = 0 -- TODO

attackEnhMod :: Power -> Character -> Int
attackEnhMod p c = Modifier.mod Modifier.Attack EnhancementMod c

attackMiscMod :: Power -> Character -> Int
attackMiscMod p c = 0 -- TODO

attack1Name :: Character -> String
attack1Name c = Power.name $ attack1Power c

attack1Power :: Character -> Power
attack1Power c = head $ atWillPowers $ attackPowers $ Character.powers c

attack1Mod :: Character -> Int
attack1Mod c = attackMod (attack1Power c) c

attack1AbilMod :: Character -> Int
attack1AbilMod c = attackAbilMod (attack1Power c) c

attack1ClassMod :: Character -> Int
attack1ClassMod c = attackClassMod (attack1Power c) c

attack1ProfMod :: Character -> Int
attack1ProfMod c = attackProfMod (attack1Power c) c

attack1FeatMod :: Character -> Int
attack1FeatMod c = attackFeatMod (attack1Power c) c

attack1EnhMod :: Character -> Int
attack1EnhMod c = attackEnhMod (attack1Power c) c

attack1MiscMod :: Character -> Int
attack1MiscMod c = attackMiscMod (attack1Power c) c

attack2Name :: Character -> String
attack2Name c = Power.name $ attack2Power c

attack2Power :: Character -> Power
attack2Power c = head $ tail $ atWillPowers $ attackPowers $ Character.powers c

attack2Mod :: Character -> Int
attack2Mod c = attackMod (attack2Power c) c

attack2AbilMod :: Character -> Int
attack2AbilMod c = attackAbilMod (attack2Power c) c

attack2ClassMod :: Character -> Int
attack2ClassMod c = attackClassMod (attack2Power c) c

attack2ProfMod :: Character -> Int
attack2ProfMod c = attackProfMod (attack2Power c) c

attack2FeatMod :: Character -> Int
attack2FeatMod c = attackFeatMod (attack2Power c) c

attack2EnhMod :: Character -> Int
attack2EnhMod c = attackEnhMod (attack2Power c) c

attack2MiscMod :: Character -> Int
attack2MiscMod c = attackMiscMod (attack2Power c) c


{----------}
{- Damage -}
{----------}
{- Overkill? -}
damageMod :: Power -> Character -> Int
damageMod p c = damageAbilMod p c
                + damageFeatMod p c
                + damageEnhMod p c
                + damageMiscMod p c

damageDesc :: Power -> Character -> String
damageDesc p c
  | Power.damage p == Nothing = ""
  | otherwise = (fromJust $ Power.damage p) ++ " + " ++ (show $ damageMod p c)

damageAbilMod :: Power -> Character -> Int
damageAbilMod p c = attackAbilMod p c

damageFeatMod :: Power -> Character -> Int
damageFeatMod p c = 0 -- TODO

damageEnhMod :: Power -> Character -> Int
damageEnhMod p c = Modifier.mod Modifier.Damage EnhancementMod c

damageMiscMod :: Power -> Character -> Int
damageMiscMod p c = 0 -- TODO

damageMisc1Mod :: Power -> Character -> Int
damageMisc1Mod p c = 0 -- TODO

damageMisc2Mod :: Power -> Character -> Int
damageMisc2Mod p c = 0 -- TODO

damage1Desc :: Character -> String
damage1Desc c = damageDesc (attack1Power c) c

damage1Mod :: Character -> Int
damage1Mod c = damageMod (attack1Power c) c

damage1AbilMod :: Character -> Int
damage1AbilMod c = damageAbilMod (attack1Power c) c

damage1FeatMod :: Character -> Int
damage1FeatMod c = damageFeatMod (attack1Power c) c

damage1EnhMod :: Character -> Int
damage1EnhMod c = damageEnhMod (attack1Power c) c

damage1Misc1Mod :: Character -> Int
damage1Misc1Mod c = damageMisc1Mod (attack1Power c) c

damage1Misc2Mod :: Character -> Int
damage1Misc2Mod c = damageMisc2Mod (attack1Power c) c

damage2Desc :: Character -> String
damage2Desc c = damageDesc (attack1Power c) c

damage2Mod :: Character -> Int
damage2Mod c = damageMod (attack1Power c) c

damage2AbilMod :: Character -> Int
damage2AbilMod c = damageAbilMod (attack1Power c) c

damage2FeatMod :: Character -> Int
damage2FeatMod c = damageFeatMod (attack1Power c) c

damage2EnhMod :: Character -> Int
damage2EnhMod c = damageEnhMod (attack1Power c) c

damage2Misc1Mod :: Character -> Int
damage2Misc1Mod c = damageMisc1Mod (attack1Power c) c

damage2Misc2Mod :: Character -> Int
damage2Misc2Mod c = damageMisc2Mod (attack1Power c) c


{----------------------}
{- Looking for a home -}
{----------------------}
size :: Character -> String
size = (Race.size . Character.race)

raceName :: Character -> String
raceName = (Race.name . Character.race)

racialAbilModifiers :: Character -> String
racialAbilModifiers c = Race.abilModifiers $ Character.race c

basicMeleeAttack c w = basicAttack c w + strAbilMod c

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

magicItem :: Int -> Character -> String
magicItem i c
  | length gear > i = gear !! i
  | otherwise = ""
  where gear = map Equipment.name $ filter (hasTag (tagFactory "Magic")) $ Character.gear c

isArmed c
  | length ((weapons . gear) c) == 0 = False
  | otherwise = True

isProficientWith c w = grantsProficiencyWith (characterClass c) w -- TODO feats

profModToPower :: Power -> Character -> Int
profModToPower p c
  | powerHasKeyword p "Weapon" == True && isArmed c = fromJust $ Equipment.proficiencyBonus $ Character.primaryWeapon c
  | otherwise = 0

classModifierFor c p =
  maximum $ 0:(map value $ filter (\mod -> (show $ Modifier.target mod) == Power.name p) $ CC.modifiers $ characterClass c)

featModifierFor c p =
  maximum $ 0:(map value $ filter (\mod -> (show $ Modifier.target mod) == Power.name p) $ concatMap Feat.modifiers $ Character.feats c)

attackBonus :: Character -> AbilityName -> Int
attackBonus c a = (basicAttack c (primaryWeapon c)) + ((abilityMod a) c)

className c = (CC.name . characterClass) c
