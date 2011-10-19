module Pontus where

import qualified CharacterClass as CC
import Modifier
import Character
import Race
import CharacterClass
import CharacterClass.Battlemind
import Level
import Skill
import Equipment


pontus = Character { Character.name = "Pontus"
                   , baseStr=11
                   , baseDex=10
                   , baseCon=17
                   , baseInt=10
                   , baseWis=14
                   , baseCha=12
                   , characterClass=battlemind [] [Skill.Heal, 
                                                   Skill.Diplomacy,
                                                   Skill.Endurance]
                   , race=human raceWisPlus2
                   , levels=[Level.level []]
                   , equippedGear=[scaleMail, lightShield]
                   , carriedGear=[]
                   }
