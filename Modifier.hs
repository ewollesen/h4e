module Modifier where

data Modifier = Modifier { name :: String
                         , target :: String -- I should be an enum
                         , value :: Int
                         } deriving (Show)


class Modifiable a where
  modifiers :: a -> [Modifier]


modFactory name target value = Modifier { Modifier.name=name
                                        , Modifier.target=target
                                        , Modifier.value=value }
