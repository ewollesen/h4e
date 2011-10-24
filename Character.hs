module Character where

import Modifier
import Taggable
import Race
import CharacterClass as CC
import Level
import Skill
import Ability
import Equipment
import Equippable
import Weapon
import Weaponlike

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
                           , gear :: [Equipment]
                           , weapons :: [Weapon]
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
skillAbilMod History= (intMod)
skillAbilMod Insight = (wisMod)
skillAbilMod Intimidate = (chaMod)
skillAbilMod Nature = (wisMod)
skillAbilMod Perception = (wisMod)
skillAbilMod Religion= (intMod)
skillAbilMod Stealth = (dexMod)
skillAbilMod Streetwise = (chaMod)
skillAbilMod Thievery = (dexMod)


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


ac :: Character -> Int
ac c = sum $ [tenPlusHalfLevel c,
              abilModForAc c,
              sum $ map value (acMods c)]

abilModForAc :: Character -> Int
abilModForAc c
  | wearingLightOrNoArmor c == True = maximum [intMod c, dexMod c]
  | otherwise                       = 0

wearingLightOrNoArmor :: Character -> Bool
wearingLightOrNoArmor c = taggedWith (gear c) heavyArmorTag == False

fortitude :: Character -> Int
fortitude c = maximum [strMod c, conMod c]
         + tenPlusHalfLevel c
         + (sum $ map value (fortMods c))

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

bloodied :: Character -> Int
bloodied c = hp c `div` 2

healingSurgeValue :: Character -> Int
healingSurgeValue c = hp c `div` 4

healingSurgesPerDay :: Character -> Int
healingSurgesPerDay c = conMod c
                        + (CC.healingSurgesPerDay . characterClass) c


speed c = 6 + armorPenalty c -- magic number, no feats or equipment


trainedBonus :: Character -> SkillName -> Int
trainedBonus c s
  | trainedSkill c s == True = 5 -- magic number :(
  | otherwise            = 0

trainedSkill :: Character -> SkillName -> Bool
trainedSkill c s = s `elem` Character.trainedSkills c

trainedSkills :: Character -> [SkillName]
trainedSkills c = concat [(CC.trainedSkills . characterClass) c
                          -- feats that train in skills
                         ]

instance Modifiable Character where
  modifiers c = concat [(Race.modifiers . race) c,
                        (CC.modifiers . characterClass) c,
                        (concatMap Equipment.modifiers (gear c)),
                        (concatMap Level.modifiers (Character.levels c))]

instance Skilled Character where
  skill c name = trainedBonus c name +
                 (skillAbilMod name) c +
                 skillArmorPenalty c name
  skillMods c name = []
  skillArmorPenalty c name
    | skillArmorPenaltyApplies name (not (wearingLightOrNoArmor c)) = armorPenalty c
    | otherwise = 0


armorPenalty c = sum $ (map value (filter (\mod -> target mod == "ArmorPenalty") (Modifier.modifiers c)))


basicMeleeAttack c w = basicAttack c w + strMod c
basicRangedAttack c w = basicAttack c w + dexMod c

basicAttack c w = halfLevel c + (weaponProficiencyBonus c w)

weaponProficiencyBonus c w
  | isProficientWith c w == True = Weapon.proficiencyBonus w
  | otherwise = 0

primaryWeapon c = head $ weapons c
secondaryWeapon c = head $ tail (weapons c)

isProficientWith c w = grantsProficiencyWith (characterClass c) w

powers c = concatMap Level.powers (levels c)