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
               | Feature
               | Utility
               deriving (Show, Eq, Ord)
$(derive[''PowerType])

data Power = Power { name :: String
                   , powerType :: PowerType
                   , keywords :: [String] -- possibly an enum
                   , uses :: PowerUses
                   , action :: Action
                   , attackAbility :: (Maybe AbilityName)
                   , attackVsDefense :: (Maybe String) -- DefenseName enum
                   } deriving (Show)


powerHasKeyword power keyword = keyword `elem` (Power.keywords power)