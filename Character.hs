module Character where

import Abilities
import Race
import CharacterClass

data Character = Character { name :: String
                           , abilities :: Abilities
                           , race :: Race
                           , characterClass :: Class
                           } deriving (Show)

