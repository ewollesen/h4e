module Modifier where

data Modifier = Modifier { name :: String
                         , modType :: String -- I should be an enum
                         , mod :: Int
                         } deriving (Show)

raceWisPlus2 = Modifier {name="+2 Wisdom (Racial)", modType="Wisdom", Modifier.mod=2}
