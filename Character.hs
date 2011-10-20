module Character where

import Modifier
import Race
import CharacterClass
import Level


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
                           } deriving (Show)

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
strMods c = filter (\mod -> modType mod == "Strength") (Modifier.modifiers c)

dexMods :: (Modifiable a) => a -> [Modifier]
dexMods c = filter (\mod -> modType mod == "Dexterity") (Modifier.modifiers c)

conMods :: (Modifiable a) => a -> [Modifier]
conMods c = filter (\mod -> modType mod == "Constitution") (Modifier.modifiers c)

intMods :: (Modifiable a) => a -> [Modifier]
intMods c = filter (\mod -> modType mod == "Intelligence") (Modifier.modifiers c)

wisMods :: (Modifiable a) => a -> [Modifier]
wisMods c = filter (\mod -> modType mod == "Wisdom") (Modifier.modifiers c)

chaMods :: (Modifiable a) => a -> [Modifier]
chaMods c = filter (\mod -> modType mod == "Charisma") (Modifier.modifiers c)


level :: Character -> Int
level c = length $ levels c

halfLevel :: Character -> Int
halfLevel c = Character.level c `div` 2

tenPlusHalfLevel :: Character -> Int
tenPlusHalfLevel c = 10 + halfLevel c

-- is initiative affected by the armor check penalty in 4E? NO.
initiative :: Character -> Int
initiative c = maximum [(intMod c), (dexMod c)] + (halfLevel c)

fortitude :: Character -> Int
fortitude c = maximum [strMod c, conMod c] + tenPlusHalfLevel c

reflex :: Character -> Int
reflex c = maximum [dexMod c, intMod c] + tenPlusHalfLevel c

will :: Character -> Int
will c = maximum [chaMod c, wisMod c] + tenPlusHalfLevel c

instance Modifiable Character where
  modifiers c = concat [(Race.modifiers . race) c, 
                        (CharacterClass.modifiers . characterClass) c,
                        (concatMap Level.modifiers (Character.levels c))]