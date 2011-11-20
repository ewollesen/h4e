module Race where

import Data.List
import Modifier


data Race = Race { name :: String
                 , baseSpeed :: Int
                 , size :: String -- should be an enum?
                 , modifiers :: [Modifier]
                 } deriving (Show)

instance Modifiable Race where
  modifiers r = Race.modifiers r

human :: ModTarget -> Race
human a = Race { Race.name="Human"
               , Race.baseSpeed=6
               , Race.size="Medium"
               , Race.modifiers=[abilModFactory a,
                                 plusOneFortitude,
                                 plusOneReflex,
                                 plusOneWill,
                                 plusOneSaveIfBloodied]
               }

abilModFactory a = modFactory desc a 2 UntypedMod
  where desc = "+2 " ++ (show a) ++ " (Racial)"

plusOneFortitude = modFactory "+1 Fortitude (Racial)" Fortitude 1 UntypedMod
plusOneReflex = modFactory "+1 Reflex (Racial)" Reflex 1 UntypedMod
plusOneWill = modFactory "+1 Will (Racial)" Will 1 UntypedMod

plusOneSaveIfBloodied = modFactory "+1 when bloodied" SavingThrow 1 UntypedMod

abilModifiers :: Race -> String
abilModifiers r = concat $ intersperse ", " $ map abilModToShortDesc $ filter isAbilModifier $ Modifier.modifiers r

isAbilModifier :: Modifier -> Bool
isAbilModifier m
  | target m `elem` [Strength, Constitution, Dexterity, Intelligence, Wisdom, Charisma] = True
  | otherwise = False

abilModToShortDesc :: Modifier -> String
abilModToShortDesc m = "+" ++ (show $ value m) ++ " " ++ (take 3 $ show $ target m)

