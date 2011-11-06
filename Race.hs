module Race where

import Modifier

data Race = Race { name :: String
                 , baseSpeed :: Int
                 , size :: String -- should be an enum?
                 , modifiers :: [Modifier]
                 } deriving (Show)


-- is there a way to generate these from a list?
raceWisPlus2 = modFactory "+2 Wisdom (Racial)" Wisdom 2 UntypedMod

racePlusOneFortitude = modFactory "+1 Fortitude (Racial)" Fortitude 1 UntypedMod
racePlusOneReflex = modFactory "+1 Reflex (Racial)" Reflex 1 UntypedMod
racePlusOneWill = modFactory "+1 Will (Racial)" Will 1 UntypedMod

human :: Modifier -> Race
human abil = Race { Race.name="Human"
                  , Race.baseSpeed=6
                  , Race.size="Medium"
                  , Race.modifiers=[abil,
                                    racePlusOneFortitude,
                                    racePlusOneReflex,
                                    racePlusOneWill]
                  }
