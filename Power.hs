{-# OPTIONS_GHC
    -XTemplateHaskell
    -XFlexibleInstances
    -XMultiParamTypeClasses
    -XFlexibleContexts
    -XUndecidableInstances #-}
module Power where

import Text.RJson
import Data.Generics.SYB.WithClass.Basics
import Data.Generics.SYB.WithClass.Derive
import Taggable
import Ability


data PowerUses = AtWill
               | Encounter
               | Daily
               deriving (Show, Eq, Ord)
$(derive[''PowerUses])

data Action = FreeAction
            | ImmediateReaction
            | Opportunity
            | ImmediateInterrupt
            | MinorAction
            | MoveAction
            | StandardAction
            deriving (Show, Eq, Ord)
$(derive[''Action])

data PowerType = Attack
               | ClassFeature
               | RacialFeature
               | PathFeature
               | DestinyFeature
               | Utility
               deriving (Show, Eq, Ord)
$(derive[''PowerType])

data Power = Power { name :: String
                   , powerType :: PowerType
                   , keywords :: [String] -- possibly an enum
                   , uses :: PowerUses
                   , action :: Action
                   , level :: Int
                   , attackAbility :: (Maybe AbilityName)
                   , attackVsDefense :: (Maybe String) -- DefenseName enum
                   , damage :: (Maybe String)
                   } deriving (Show, Eq)

instance Ord Power where
  compare p1 p2
    | levelsMatch == False = compare (Power.level p2) (Power.level p1)
    | usesMatch == False = compare (uses p2) (uses p1)
    | otherwise = compare (Power.name p1) (Power.name p2)
    where levelsMatch = (Power.level p2) == (Power.level p1)
          usesMatch = (uses p2) == (uses p1)


powerHasKeyword power keyword = keyword `elem` (Power.keywords power)