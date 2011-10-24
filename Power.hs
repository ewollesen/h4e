module Power where

import Taggable
import Ability


data Power = Power { name :: String
                   , hit :: String
                   , attackAbility :: AbilityName
                   , attackVsDefense :: String -- DefenseName
                   } deriving (Show)
