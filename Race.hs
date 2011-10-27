module Race where

import Modifier

data Race = Race { name :: String
                 , baseSpeed :: Int
                 , modifiers :: [Modifier]
                 } deriving (Show)


-- is there a way to generate these from a list?
raceConPlus2 = modFactory "+2 Constitution (Racial)" "Constitution" 2
raceWisPlus2 = modFactory "+2 Wisdom (Racial)" "Wisdom" 2

racePlusOneFortitude = modFactory "+1 Fortitude (Racial)" "Fortitude" 1
racePlusOneReflex = modFactory "+1 Reflex (Racial)" "Reflex" 1
racePlusOneWill = modFactory "+1 Will (Racial)" "Will" 1

human :: Modifier -> Race
human abil = Race { Race.name="human"
                  , Race.baseSpeed=6
                  , Race.modifiers=[abil,
                                    racePlusOneFortitude,
                                    racePlusOneReflex,
                                    racePlusOneWill]
                  }