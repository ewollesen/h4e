import qualified CharacterClass as CC
import Modifier
import Character
import Race
import CharacterClass
import CharacterClass.Battlemind 
import Level

statMod :: Int -> Int
statMod x = x `div` 2 - 5

attackMod :: Int -> Int
attackMod lvl = lvl `div` 2


pontus = Character { Character.name = "Pontus"
                   , baseStr=11
                   , baseDex=10
                   , baseCon=17
                   , baseInt=10
                   , baseWis=14
                   , baseCha=12
                   , characterClass=battlemind
                   , race=human raceWisPlus2
                   , levels=[Level.level]
                   }
