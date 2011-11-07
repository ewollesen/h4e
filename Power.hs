module Power where

import Taggable
import Ability


data Power = Power { name :: String
                   , attackFeatureOrUtility :: String -- enum
                   , keywords :: [String] -- possibly an enum
                   , uses :: String -- Uses enum
                   , action :: String -- enum
                   , range :: String -- ? am I needed?
                   , attackAbility :: AbilityName
                   , attackVsDefense :: String -- DefenseName enum
                   , target :: String
                   , hit :: String
                   , miss :: String
                   , trigger :: String
                   , effect :: String
                   } deriving (Show)


powerHasKeyword power keyword = keyword `elem` (Power.keywords power)