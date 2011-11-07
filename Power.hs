module Power where

import Taggable
import Ability


data Power = Power { name :: String
                   , hit :: String
                   , attackAbility :: AbilityName
                   , attackVsDefense :: String -- DefenseName enum
                   , uses :: String -- Uses enum
                   , effect :: String
                   , action :: String -- enum
                   , target :: String
                   , trigger :: String
                   , miss :: String
                   , attackFeatureOrUtility :: String -- enum
                   , keywords :: [String]
                   , range :: String -- ?
                   -- tags
                   } deriving (Show)

