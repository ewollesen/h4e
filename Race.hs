module Race where

import Modifier

data Race = Race { name :: String
                 , modifiers :: [Modifier]
                 } deriving (Show)

wisModifiers :: Race -> [Modifier]
wisModifiers r = filter (\mod -> modType mod == "Wisdom") (modifiers r)
