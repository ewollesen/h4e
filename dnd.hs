-- Inputs
import qualified CharacterClass as CC
import Modifier
import Character
import Abilities
import qualified Race

statMod :: Int -> Int
statMod x = x `div` 2 - 5

attackMod :: Int -> Int
attackMod lvl = lvl `div` 2

level :: Character -> Int
level c = 1

halfLevel :: Character -> Int
halfLevel c = level c `div` 2

tenPlusHalfLevel :: Character -> Int
tenPlusHalfLevel c = 10 + halfLevel c

-- is initiative affected by the armor check penalty in 4E?
initiative :: Character -> Int
initiative c = maximum [(intMod c), (dexMod c)] + (halfLevel c)

fortitude :: Character -> Int
fortitude c = maximum [strMod c, conMod c] + tenPlusHalfLevel c

reflex :: Character -> Int
reflex c = maximum [dexMod c, intMod c] + tenPlusHalfLevel c

will :: Character -> Int
will c = maximum [chaMod c, wisMod c] + tenPlusHalfLevel c

abilMod x = x `div` 2 - 5

wisMod :: Character -> Int
wisMod c = sum $ (abilMod . wis . abilities) c:(map Modifier.mod (wisModifiers c))

wisModifiers :: Character -> [Modifier]
wisModifiers c = concat [(Race.wisModifiers . race) c] --, (CC.wisModifiers . characterClass) c]

dexMod :: Character -> Int
dexMod c = (abilMod . dex . abilities) c

conMod :: Character -> Int
conMod c = (abilMod . con . abilities) c

intMod :: Character -> Int
intMod c = (abilMod . int . abilities) c

strMod :: Character -> Int
strMod c = (abilMod . str . abilities) c

chaMod :: Character -> Int
chaMod c = (abilMod . cha . abilities) c
 
pontus = Character {Character.name = "Pontus", abilities = Abilities {str=11, dex=11, con=17, int=11, wis=14, cha=14}, race = Race.Race {Race.name="Human", Race.modifiers=[raceWisPlus2]}} -- this modifies the wisMod, instead of the wis, oops!
