module Modifier where

data Modifier = Modifier { name :: String
                         , modType :: String -- I should be an enum
                         , value :: Int
                         } deriving (Show)


class Modifiable a where
  modifiers :: a -> [Modifier]


modFactory name modType value = Modifier { Modifier.name=name
                                         , Modifier.modType=modType
                                         , Modifier.value=value }
