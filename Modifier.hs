{-# OPTIONS_GHC
    -XTemplateHaskell
    -XFlexibleInstances
    -XMultiParamTypeClasses
    -XFlexibleContexts
    -XUndecidableInstances #-}
module Modifier where

import Text.RJson
import Data.Generics.SYB.WithClass.Basics
import Data.Generics.SYB.WithClass.Derive


data ModType = AbilityMod
             | ArmorMod
             | ClassMod
             | EnhancementMod
             | FeatMod
             | ShieldMod
             | UntypedMod
             deriving (Show, Eq)
$(derive[''ModType])

data ModTarget = ArmorClass
               | ArmorSpeed
               | ArmorSkill
               | Fortitude
               | Reflex
               | Will
               | Strength
               | Constitution
               | Dexterity
               | Intelligence
               | Wisdom
               | Charisma
               | HitPoints
               | Initiative
               | Speed
               {- skills
               | Acrobatics
               | Arcana
               | Athletics
               | Bluff
               | Diplomacy
               | Dungeoneering
               | Endurance
               | Heal
               | History
               | Insight
               | Intimidate
               | Nature
               | Perception
               | Religion
               | Stealth
               | Streetwise
               | Thievery
               -}
               deriving (Show, Eq)
$(derive[''ModTarget])

data Modifier = Modifier { name :: String
                         , target :: ModTarget
                         , value :: Int
                         , modType :: ModType
                         } deriving (Show)

class Modifiable a where
  modifiers :: a -> [Modifier]

  modsByType :: a -> ModType -> [Modifier]
  modsByType a t = filter (\mod -> modType mod == t) $ modifiers a

  modsByTarget :: a -> ModTarget -> [Modifier]
  modsByTarget a t = filter (\mod -> target mod == t) $ modifiers a

  modsByTargetAndType :: a -> ModTarget -> ModType -> [Modifier]
  modsByTargetAndType a desiredTarget desiredModType =
    filter (\mod -> target mod == desiredTarget && modType mod == desiredModType) $ modifiers a

modFactory name target value modType = Modifier { Modifier.name=name
                                                , Modifier.target=target
                                                , Modifier.value=value
                                                , Modifier.modType=modType
                                                }

instance Eq Modifier where
  x == y = target x == target y && modType x == modType y && value x == value y

instance Ord Modifier where
  x < y = target x == target y && modType x == modType y && value x < value y



modTypeToString :: String -> String
modTypeToString mt = mt
modTargetToString = modTypeToString

instance Data ToJsonD ModType => ToJson ModType where
  toJson = (enumToJson modTypeToString)

instance (Data FromJsonD ModType, TranslateField ModType) => FromJson ModType where
  fromJson = (enumFromJson modTypeToString)

instance Data ToJsonD ModTarget => ToJson ModTarget where
  toJson = (enumToJson modTargetToString)

instance (Data FromJsonD ModTarget, TranslateField ModTarget) => FromJson ModTarget where
  fromJson = (enumFromJson modTargetToString)
