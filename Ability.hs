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


abilityNameToString :: String -> String
abilityNameToString a = a

instance Data ToJsonD AbilityName => ToJson AbilityName where
  toJson = (enumToJson abilityNameToString)

instance (Data FromJsonD AbilityName, TranslateField AbilityName) => FromJson AbilityName where
  fromJson = (abilityNameFromJson abilityNameToString)

abilityNameFromJson :: (Data FromJsonD a, Data TranslateFieldD a) => (String -> String) -> a -> JsonData -> Either String a
abilityNameFromJson transform dummy (JDString s) = case s of
  "Strength" -> Right Strength
  "Constitution" -> Right Constitution
  "Dexterity" -> Right Dexterity
  "Intelligence" -> Right Intelligence
  "Wisdom" -> Right Wisdom
  "Charisma" -> Right Charisma
  _ -> Left "Error in ability mapping"

$(derive[''AbilityName])
