module Race where

import Modifier

data Race = Race { name :: String
                 , modifiers :: [Modifier]
                 } deriving (Show)


-- is there a way to generate these from a list?
raceConPlus2 = modFactory "+2 Constitution (Racial)" "Constitution" 2
raceWisPlus2 = modFactory "+2 Wisdom (Racial)" "Wisdom" 2


human :: Modifier -> Race
human abil = Race { Race.name="human"
                  , Race.modifiers=[abil]
                  }