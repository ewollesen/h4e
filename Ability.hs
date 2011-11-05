{-# OPTIONS_GHC
    -XTemplateHaskell
    -XFlexibleInstances
    -XMultiParamTypeClasses
    -XFlexibleContexts
    -XUndecidableInstances #-}

module Ability where

import Text.RJson
import Data.Generics.SYB.WithClass.Basics
import Data.Generics.SYB.WithClass.Derive

data AbilityName = Strength
                 | Dexterity
                 | Constitution
                 | Intelligence
                 | Wisdom
                 | Charisma
                 deriving (Show, Eq, Enum)

$(derive[''AbilityName])

abilityNameToString :: String -> String
abilityNameToString a = a

instance Data ToJsonD AbilityName => ToJson AbilityName where
  toJson = (enumToJson abilityNameToString)

instance (Data FromJsonD AbilityName, TranslateField AbilityName) => FromJson AbilityName where
  fromJson = (enumFromJson abilityNameToString)
