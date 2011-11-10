module Race (Race.Race,
             Race.name,
             Race.size,
             Race.baseSpeed,
             Race.human) where

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
                                 plusOneWill]
               }

abilModFactory a = modFactory desc a 2 UntypedMod
  where desc = "+2 " ++ (show a) ++ " (Racial)"

plusOneFortitude = modFactory "+1 Fortitude (Racial)" Fortitude 1 UntypedMod
plusOneReflex = modFactory "+1 Reflex (Racial)" Reflex 1 UntypedMod
plusOneWill = modFactory "+1 Will (Racial)" Will 1 UntypedMod
