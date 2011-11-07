module Character where

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
import Weapon
import Weaponlike
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
                           , weapons :: [Weapon]
                           , xp :: Int
                           , languages :: [String]
                           , adventuringCompanyOrOtherAffiliations :: String
                           } deriving (Show)



ability Ability.Strength = (str)
ability Ability.Dexterity = (dex)
ability Ability.Constitution = (con)
ability Ability.Intelligence = (int)
ability Ability.Wisdom = (wis)
ability Ability.Charisma = (cha)

abilityMod Ability.Strength = (strMod)
abilityMod Ability.Dexterity = (dexMod)
abilityMod Ability.Constitution = (conMod)
abilityMod Ability.Intelligence = (intMod)
abilityMod Ability.Wisdom = (wisMod)
abilityMod Ability.Charisma = (chaMod)

skillAbilMod Acrobatics = (dexMod)
skillAbilMod Arcana = (intMod)
skillAbilMod Athletics = (strMod)
skillAbilMod Bluff = (chaMod)
skillAbilMod Diplomacy = (chaMod)
skillAbilMod Dungeoneering = (wisMod)
skillAbilMod Endurance = (conMod)
skillAbilMod Heal = (wisMod)
skillAbilMod History = (intMod)
skillAbilMod Insight = (wisMod)
skillAbilMod Intimidate = (chaMod)
skillAbilMod Nature = (wisMod)
skillAbilMod Perception = (wisMod)
skillAbilMod Religion = (intMod)
skillAbilMod Stealth = (dexMod)
skillAbilMod Streetwise = (chaMod)
skillAbilMod Thievery = (dexMod)

passiveInsight :: Character -> Int
passiveInsight c = 10 + skill c Insight

passivePerception :: Character -> Int
passivePerception c = 10 + skill c Perception


str :: Character -> Int
str c = sum $ baseStr c:(map value (Character.strMods c))

dex :: Character -> Int
dex c = sum $ baseDex c:(map value (Character.dexMods c))

con :: Character -> Int
con c = sum $ baseCon c:(map value (Character.conMods c))

int :: Character -> Int
int c = sum $ baseInt c:(map value (Character.intMods c))

wis :: Character -> Int
wis c = sum $ baseWis c:(map value (Character.wisMods c))

cha :: Character -> Int
cha c = sum $ baseCha c:(map value (Character.chaMods c))


abilMod x = x `div` 2 - 5

wisMod :: Character -> Int
wisMod c = (abilMod . wis) c

dexMod :: Character -> Int
dexMod c = (abilMod . dex) c

conMod :: Character -> Int
conMod c = (abilMod . con) c

intMod :: Character -> Int
intMod c = (abilMod . int) c

strMod :: Character -> Int
strMod c = (abilMod . str) c

chaMod :: Character -> Int
chaMod c = (abilMod . cha) c


strMods :: (Modifiable a) => a -> [Modifier]
strMods c = filter (\mod -> Modifier.target mod == Modifier.Strength) (Modifier.modifiers c)

dexMods :: (Modifiable a) => a -> [Modifier]
dexMods c = filter (\mod -> Modifier.target mod == Modifier.Dexterity) (Modifier.modifiers c)

conMods :: (Modifiable a) => a -> [Modifier]
conMods c = filter (\mod -> Modifier.target mod == Modifier.Constitution) (Modifier.modifiers c)

intMods :: (Modifiable a) => a -> [Modifier]
intMods c = filter (\mod -> Modifier.target mod == Modifier.Intelligence) (Modifier.modifiers c)

wisMods :: (Modifiable a) => a -> [Modifier]
wisMods c = filter (\mod -> Modifier.target mod == Modifier.Wisdom) (Modifier.modifiers c)

chaMods :: (Modifiable a) => a -> [Modifier]
chaMods c = filter (\mod -> Modifier.target mod == Modifier.Charisma) (Modifier.modifiers c)


fortMods :: (Modifiable a) => a -> [Modifier]
fortMods c = filter (\mod -> Modifier.target mod == Fortitude) (Modifier.modifiers c)

refMods :: (Modifiable a) => a -> [Modifier]
refMods c = filter (\mod -> Modifier.target mod == Reflex) (Modifier.modifiers c)

willMods :: (Modifiable a) => a -> [Modifier]
willMods c = filter (\mod -> Modifier.target mod == Will) (Modifier.modifiers c)

acMods :: (Modifiable a) => a -> [Modifier]
acMods c = modsByTarget c ArmorClass

enhModToTarget c t =
  maximum $ 0:(map value $ modsByTargetAndType c t EnhancementMod)
enhModToAC c = enhModToTarget c ArmorClass
enhModToFortitude c = enhModToTarget c Fortitude
enhModToReflex c = enhModToTarget c Reflex
enhModToWill c = enhModToTarget c Will


firstMod mods
  | length mods > 0 = value $ head mods
  | otherwise = 0

secondMod mods
  | length mods > 1 = value $ head $ tail mods
  | otherwise = 0

nonMiscACModTypes = [AbilityMod, ArmorMod, ClassMod, EnhancementMod, FeatMod]
miscACMods c =
  filter (\mod -> modType mod `notElem` nonMiscACModTypes) acMods
  where acMods = Character.acMods c
firstMiscACMod c = firstMod $ miscACMods c
secondMiscACMod c = secondMod $ miscACMods c

nonMiscFortitudeModTypes = [AbilityMod, ClassMod, EnhancementMod, FeatMod]
miscFortitudeMods c =
  filter (\mod -> modType mod `notElem` nonMiscFortitudeModTypes) $ fortMods c
  where acMods = Character.acMods c
firstMiscFortitudeMod c = firstMod $ miscFortitudeMods c
secondMiscFortitudeMod c = secondMod $ miscFortitudeMods c

nonMiscReflexModTypes = nonMiscFortitudeModTypes
miscReflexMods c =
  filter (\mod -> modType mod `notElem` nonMiscReflexModTypes) $refMods c
  where acMods = Character.acMods c
firstMiscReflexMod c = firstMod $ miscReflexMods c
secondMiscReflexMod c = secondMod $ miscReflexMods c

nonMiscWillModTypes = nonMiscFortitudeModTypes
miscWillMods c =
  filter (\mod -> modType mod `notElem` nonMiscWillModTypes) $ willMods c
  where acMods = Character.acMods c
firstMiscWillMod c = firstMod $ miscWillMods c
secondMiscWillMod c = secondMod $ miscWillMods c

className c = (CC.name . characterClass) c

level :: Character -> Int
level c = length $ levels c

halfLevel :: Character -> Int
halfLevel c = Character.level c `div` 2

tenPlusHalfLevel :: Character -> Int
tenPlusHalfLevel c = 10 + halfLevel c

-- is initiative affected by the armor penalty in 4E? NO.
-- add other mods, feats, powers, equipment
initiative :: Character -> Int
initiative c = maximum [(intMod c), (dexMod c)] + (halfLevel c)

miscModToInit c = sum $ map value $ modsFor c Initiative -- primitive

modsFor c t = filter (\mod -> Modifier.target mod == t) $ Modifier.modifiers c

feats :: Character -> [Feat]
feats c = concatMap Level.feats (levels c)

ac :: Character -> Int
ac c = sum $ [tenPlusHalfLevel c,
              abilModForAc c,
              sum $ map value (Character.acMods c)]

abilModForAc :: Character -> Int
abilModForAc c
  | wearingLightOrNoArmor c == True = maximum [intMod c, dexMod c]
  | otherwise                       = 0

wearingLightOrNoArmor :: Character -> Bool
wearingLightOrNoArmor c = taggedWith (gear c) heavyArmorTag == False

armorOrAbilityModToAC :: Character -> Int
armorOrAbilityModToAC c
  | wearingLightOrNoArmor c == True = abilModForAc c
  | otherwise = sum $ map value (Character.acMods c) -- I probably need to be more specific

classModToAC c = sum $ map Modifier.value $ (Character.acMods . Character.characterClass) c

featModToAC c = sum $ map Modifier.value $ filter (\mod -> Modifier.target mod == ArmorClass) (concatMap Feat.modifiers (Character.feats c))

fortitude :: Character -> Int
fortitude c = abilityModToFortitude c
              + tenPlusHalfLevel c
              + (sum $ map value (fortMods c))

abilityModToFortitude c = maximum [strMod c, conMod c]

classModToFortitude c
   | length mods == 0 = 0
   | otherwise = maximum $ map value mods
  where
    mods = fortMods $ Character.characterClass c

featModToFortitude c
  | length mods == 0 = 0
  | otherwise = maximum $ map value mods
  where mods = modsFromFeatsWithTarget c Fortitude

modsFromFeatsWithTarget c t =
  filter (\mod -> Modifier.target mod == t) $ featModifiers c

featModifiers c = concatMap Feat.modifiers (Character.feats c)

abilityModToReflex c = maximum [dexMod c, intMod c]

classModToReflex c
   | length mods == 0 = 0
   | otherwise = maximum $ map value mods
  where
    mods = refMods $ Character.characterClass c

featModToReflex c
  | length mods == 0 = 0
  | otherwise = maximum $ map value mods
  where mods = modsFromFeatsWithTarget c Reflex

abilityModToWill c = maximum [wisMod c, chaMod c]

classModToWill c
   | length mods == 0 = 0
   | otherwise = maximum $ map value mods
  where
    mods = willMods $ Character.characterClass c

featModToWill c
  | length mods == 0 = 0
  | otherwise = maximum $ map value mods
  where mods = modsFromFeatsWithTarget c Will



reflex :: Character -> Int
reflex c = maximum [intMod c, dexMod c]
         + tenPlusHalfLevel c
         + (sum $ map value (refMods c))

will :: Character -> Int
will c = maximum [chaMod c, wisMod c]
         + tenPlusHalfLevel c
         + (sum $ map value (willMods c))

hp :: Character -> Int
hp c = con c
       + (hpAtFirstLevel . characterClass) c
       + ((hpPerLevelGained . characterClass) c * (Character.level c - 1))
       + (sum $ map value (hpMods c))

hpMods c = filter (\mod -> Modifier.target mod == HitPoints) (Modifier.modifiers c)

bloodied :: Character -> Int
bloodied c = hp c `div` 2

healingSurgeValue :: Character -> Int
healingSurgeValue c = hp c `div` 4

healingSurgesPerDay :: Character -> Int
healingSurgesPerDay c = conMod c
                        + (CC.healingSurgesPerDay . characterClass) c


speed c = (baseSpeed (race c)) + (sum $ (map value (speedMods c)))
speedMods c = filter (\mod -> Modifier.target mod == Speed) (Modifier.modifiers c)
armorSpeedMod c = sum $ map value $ filter (\mod -> Modifier.target mod == Speed) (concatMap Equipment.modifiers (gear c)) -- not entirely accurate, add a filter for tagged with armor I guess?
itemSpeedMod c
  | length mods > 0 = value $ maximum mods
  | otherwise = 0
  where
    nonArmor = filter (\gear -> isTaggedWith gear armorTag == False) $ gear c
    mods = concat [modsByTarget x Speed | x <- nonArmor]

nonMiscSpeedModTypes = [ArmorMod]
miscSpeedMod c
  | length mods > 0 = value $ maximum $ mods
  | otherwise = 0
  where speedMods = modsByTarget c Speed
        mods = filter (\mod -> modType mod `notElem` nonMiscSpeedModTypes) speedMods

seventeenFeats c = buildSeventeenFeats $ map Feat.name $ Character.feats c
buildSeventeenFeats f
  | length f < 17 = buildSeventeenFeats (f ++ [""])
  | otherwise = f

sixAtWillPowers c = buildSixAtWillPowers $ map Power.name $ atWillPowers $ Character.powers c
buildSixAtWillPowers p
  | length p < 6 = buildSixAtWillPowers (p ++ [""])
  | otherwise = p

sixEncounterPowers c = buildSixEncounterPowers $ map Power.name $ encounterPowers $ Character.powers c
buildSixEncounterPowers p
  | length p < 6 = buildSixEncounterPowers (p ++ [""])
  | otherwise = p

sixDailyPowers c = buildSixDailyPowers $ map Power.name $ dailyPowers $ Character.powers c
buildSixDailyPowers p
  | length p < 6 = buildSixDailyPowers (p ++ [""])
  | otherwise = p

eightUtilityPowers c = buildEightUtilityPowers $ map Power.name $ utilityPowers $ Character.powers c
buildEightUtilityPowers p
  | length p < 8 = buildEightUtilityPowers (p ++ [""])
  | otherwise = p

instance Modifiable Character where
  modifiers c = concat [(Race.modifiers . race) c,
                        (CC.modifiers . characterClass) c,
                        (concatMap Feat.modifiers (Character.feats c)),
                        (concatMap Equipment.modifiers (gear c)),
                        (concatMap Level.modifiers (Character.levels c))]


instance Skilled Character where
  skill c name = trainedBonus c name +
                 halfLevel c +
                 (skillAbilMod name) c +
                 skillArmorCheckPenalty c name
  skillMods c name = []
  skillArmorCheckPenalty c name
    | skillArmorCheckPenaltyApplies name (not (wearingLightOrNoArmor c)) = armorCheckPenalty c
    | otherwise = 0
  trainedSkills c = concat [(CC.trainedSkills . characterClass) c
                            -- feats that train in skills
                           ]
  trainedSkill c s = s `elem` Skill.trainedSkills c
  skillAbilModPlusHalfLevel c s = halfLevel c + ((skillAbilMod s) c)



armorCheckPenalty c = sum $ (map value (armorCheckPenaltyMods c))
armorCheckPenaltyMods c = filter (\mod -> Modifier.target mod == ArmorSkill) (Modifier.modifiers c)
basicMeleeAttack c w = basicAttack c w + strMod c
basicRangedAttack c w = basicAttack c w + dexMod c

basicAttack c w = halfLevel c + (weaponProficiencyBonus c w)

weaponProficiencyBonus c w
  | isProficientWith c w == True = Weapon.proficiencyBonus w
  | otherwise = 0

primaryWeapon c = head $ weapons c
secondaryWeapon c = head $ tail $ weapons c
isArmed c
  | length (weapons c) == 0 = False
  | otherwise = True

isProficientWith c w = grantsProficiencyWith (characterClass c) w -- TODO feats

powers c = concatMap Level.powers (levels c) -- TODO racial & class
powersByUses powers uses =
  filter (\p -> Power.uses p == uses) powers
atWillPowers powers = powersByUses powers "At-Will"
attackPowers powers =
  filter (\p -> Power.attackFeatureOrUtility p == "Attack") powers
encounterPowers powers = powersByUses powers "Encounter"
dailyPowers powers = powersByUses powers "Daily"
utilityPowers powers = powersByUses powers "Utility"

attackBonus :: Character -> AbilityName -> Int
attackBonus c a = (basicAttack c (primaryWeapon c)) + ((abilityMod a) c)

firstLanguage c
  | length langs == 0 = ""
  | otherwise = langs !! 0
  where
    langs = languages c

secondLanguage c
  | length langs < 2 = ""
  | otherwise = langs !! 1
  where
    langs = languages c

thirdLanguage c
  | length langs < 3 = ""
  | otherwise = langs !! 2
  where
    langs = languages c

firstAttack c = head $ attackPowers $ atWillPowers $ Character.powers c
secondAttack c = head $ tail $ attackPowers $ atWillPowers $ Character.powers c

proficiencyModForPower c p
  | powerHasKeyword p "Weapon" == True && isArmed c = Weapon.proficiencyBonus $ Character.primaryWeapon c
  | otherwise = 0