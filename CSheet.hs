module CSheet where

import Text.Printf
import Character
import Skill
import Ability
import Power

printRow :: String -> String -> IO ()
printRow k v = printf "%-20s %2s\n" k v

printSkills c = mapM_ (printSkill c) skillNames

printSkill :: Character -> SkillName -> IO ()
printSkill c s = printRow ((show s) ++ ":") (show $ skill c s)

printRule :: String -> IO ()
printRule s = printRow "----------------------------" ""

-- printAbilities c = mapM_ (printAbility c) abilityNames
printAbility :: Character -> AbilityName -> IO ()
printAbility c a = printRow ((show a) ++ ":")
                   (show ((ability a) c) ++
                    " (" ++ (printf "+%d" $ (abilityMod a) c ) ++ ")")

printPowers :: Character -> IO ()
printPowers c = mapM_ (printPower) (Character.powers c)

printPower :: Power -> IO ()
printPower p = do
  printRow (Power.name p) ""
  printRow "Attack:" ((show $ attackAbility p) ++ " vs " ++ (attackVsDefense p))
  printRow "Hit:" (hit p)

csheet :: Character -> IO ()
csheet c = do
  printAbility c Strength
  printAbility c Constitution
  printAbility c Dexterity
  printAbility c Intelligence
  printAbility c Wisdom
  printAbility c Charisma
  printRule ""
  printRow "Max HP:" (show $ hp c)
  printRow "Bloodied:" (show $ bloodied c)
  printRow "Healing surge value:" (show $ healingSurgeValue c)
  printRow "Healing surges/day:" (show $ healingSurgesPerDay c)
  printRule ""
  printRow "Initiative:" (show $ initiative c)
  printRule ""
  printRow "AC:" (show $ ac c)
  printRow "Fortitude:" (show $ fortitude c)
  printRow "Reflex:" (show $ reflex c)
  printRow "Willpower:" (show $ will c)
  printRule ""
  printRow "Speed:" (show $ speed c)
  printRule ""
  printRow "Passive insight:" (show $ skillPassiveInsight c)
  printRow "Passive perception:" (show $ skillPassivePerception c)
  printRule ""
  printRow "Basic attack melee:" (show $ basicMeleeAttack c (primaryWeapon c))
  printRow "Basic attack ranged:" (show $ basicRangedAttack c (secondaryWeapon c))
  printRule ""
  printPowers c
  printRule ""
  printSkills c