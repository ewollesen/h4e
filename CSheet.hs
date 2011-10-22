module CSheet where

import Text.Printf
import Character
import Skill

printRow :: String -> String -> IO ()
printRow k v = printf "%-20s %2s\n" k v

printSkills c = mapM_ (printSkill c) skillNames

printSkill :: Character -> SkillName -> IO ()
printSkill c s = printRow ((show s) ++ ":") (show $ skill c s)

printRule :: String -> IO ()
printRule s = printRow "-----------------------" ""

-- printAbilities c = mapM_ (printAbility c) abilityNames
-- printAbility c a = printRow ((show a) ++ ":") (show $ ability c a) ++ " (" ++ (show $ abilityMod c a) ++ ")"

csheet :: Character -> IO ()
csheet c = do
  printRow "Strength:" (show $ str c)
  printRow "Constitution:" (show $ con c)
  printRow "Dexterity:" (show $ dex c)
  printRow "Intelligence:" (show $ int c)
  printRow "Wisdom:" (show $ wis c)
  printRow "Charisma:" (show $ cha c)
  printRule ""
  printRow "Initiative:" (show $ initiative c)
  printRule ""
  printRow "AC:" (show $ ac c)
  printRow "Fortitude:" (show $ fortitude c)
  printRow "Reflex:" (show $ reflex c)
  printRow "Willpower:" (show $ will c)
  printRule ""
  printRow "HP:" (show $ hp c)
  printRow "Bloodied:" (show $ bloodied c)
  printRow "Healing surges/day:" (show $ healingSurgesPerDay c)
  printRow "Healing surge value:" (show $ healingSurgeValue c)
  printRule ""
  printSkills c