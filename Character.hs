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
                           } deriving (Show)



ability Strength = (str)
ability Dexterity = (dex)
ability Constitution = (con)
ability Intelligence = (int)
ability Wisdom = (wis)
ability Charisma = (cha)

abilityMod Strength = (strMod)
abilityMod Dexterity = (dexMod)
abilityMod Constitution = (conMod)
abilityMod Intelligence = (intMod)
abilityMod Wisdom = (wisMod)
abilityMod Charisma = (chaMod)

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

skillAbil Acrobatics = Dexterity
skillAbil Arcana = Intelligence
skillAbil Athletics = Strength
skillAbil Bluff = Charisma
skillAbil Diplomacy = Charisma
skillAbil Dungeoneering = Wisdom
skillAbil Endurance = Constitution
skillAbil Heal = Wisdom
skillAbil History = Intelligence
skillAbil Insight = Wisdom
skillAbil Intimidate = Charisma
skillAbil Nature = Wisdom
skillAbil Perception = Wisdom
skillAbil Religion = Intelligence
skillAbil Stealth = Dexterity
skillAbil Streetwise = Charisma
skillAbil Thievery = Dexterity

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
strMods c = filter (\mod -> target mod == "Strength") (Modifier.modifiers c)

dexMods :: (Modifiable a) => a -> [Modifier]
dexMods c = filter (\mod -> target mod == "Dexterity") (Modifier.modifiers c)

conMods :: (Modifiable a) => a -> [Modifier]
conMods c = filter (\mod -> target mod == "Constitution") (Modifier.modifiers c)

intMods :: (Modifiable a) => a -> [Modifier]
intMods c = filter (\mod -> target mod == "Intelligence") (Modifier.modifiers c)

wisMods :: (Modifiable a) => a -> [Modifier]
wisMods c = filter (\mod -> target mod == "Wisdom") (Modifier.modifiers c)

chaMods :: (Modifiable a) => a -> [Modifier]
chaMods c = filter (\mod -> target mod == "Charisma") (Modifier.modifiers c)


fortMods :: (Modifiable a) => a -> [Modifier]
fortMods c = filter (\mod -> target mod == "Fortitude") (Modifier.modifiers c)

refMods :: (Modifiable a) => a -> [Modifier]
refMods c = filter (\mod -> target mod == "Reflex") (Modifier.modifiers c)

willMods :: (Modifiable a) => a -> [Modifier]
willMods c = filter (\mod -> target mod == "Will") (Modifier.modifiers c)

acMods :: (Modifiable a) => a -> [Modifier]
acMods c = filter (\mod -> target mod == "AC") (Modifier.modifiers c)


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

featModToAC c = sum $ map Modifier.value $ filter (\mod -> target mod == "AC") (concatMap Feat.modifiers (Character.feats c))

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
  where mods = modsFromFeatsWithTarget c "Fortitude"

modsFromFeatsWithTarget c t =
  filter (\mod -> target mod == t) $ featModifiers c

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
  where mods = modsFromFeatsWithTarget c "Reflex"

abilityModToWill c = maximum [wisMod c, chaMod c]

classModToWill c
   | length mods == 0 = 0
   | otherwise = maximum $ map value mods
  where
    mods = willMods $ Character.characterClass c

featModToWill c
  | length mods == 0 = 0
  | otherwise = maximum $ map value mods
  where mods = modsFromFeatsWithTarget c "Will"



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

hpMods c = filter (\mod -> target mod == "Hit Points") (Modifier.modifiers c)

bloodied :: Character -> Int
bloodied c = hp c `div` 2

healingSurgeValue :: Character -> Int
healingSurgeValue c = hp c `div` 4

healingSurgesPerDay :: Character -> Int
healingSurgesPerDay c = conMod c
                        + (CC.healingSurgesPerDay . characterClass) c


speed c = (baseSpeed (race c)) + (sum $ (map value (speedMods c)))
speedMods c = filter (\mod -> target mod == "Speed") (Modifier.modifiers c)
armorSpeedMod c = sum $ map value $ filter (\mod -> target mod == "Speed") (concatMap Equipment.modifiers (gear c)) -- not entirely accurate, add a filter for tagged with armor I guess?

seventeenFeats c = buildSeventeenFeats $ map Feat.name $ Character.feats c
buildSeventeenFeats f
  | length f < 17 = buildSeventeenFeats (f ++ [""])
  | otherwise = f

sixAtWillPowers c = buildSixAtWillPowers $ map Power.name $ Character.atWillPowers c
buildSixAtWillPowers p
  | length p < 6 = buildSixAtWillPowers (p ++ [""])
  | otherwise = p

sixEncounterPowers c = buildSixEncounterPowers $ map Power.name $ Character.encounterPowers c
buildSixEncounterPowers p
  | length p < 6 = buildSixEncounterPowers (p ++ [""])
  | otherwise = p

sixDailyPowers c = buildSixDailyPowers $ map Power.name $ Character.dailyPowers c
buildSixDailyPowers p
  | length p < 6 = buildSixDailyPowers (p ++ [""])
  | otherwise = p

eightUtilityPowers c = buildEightUtilityPowers $ map Power.name $ Character.utilityPowers c
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
armorCheckPenaltyMods c = filter (\mod -> target mod == "ArmorCheckPenalty") (Modifier.modifiers c)
basicMeleeAttack c w = basicAttack c w + strMod c
basicRangedAttack c w = basicAttack c w + dexMod c

basicAttack c w = halfLevel c + (weaponProficiencyBonus c w)

weaponProficiencyBonus c w
  | isProficientWith c w == True = Weapon.proficiencyBonus w
  | otherwise = 0

primaryWeapon c = head $ weapons c
secondaryWeapon c = head $ tail $ weapons c

isProficientWith c w = grantsProficiencyWith (characterClass c) w -- TODO feats

powers c = concatMap Level.powers (levels c) -- TODO racial
powersByUses c u = filter (\p -> Power.uses p == u) $ Character.powers c
atWillPowers c = powersByUses c "At-Will"
encounterPowers c = powersByUses c "Encounter"
dailyPowers c = powersByUses c "Daily"
utilityPowers c = powersByUses c "Utility" -- does this even make sense?

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
