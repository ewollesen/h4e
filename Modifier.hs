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
import Data.List

data ModType = AbilityMod
             | ArmorMod
             | ClassMod
             | EnhancementMod
             | ItemMod
             | FeatMod
             | ShieldMod
             | LevelMod
             | ProficiencyMod
             | RacialMod
             | PowerMod
             | UntypedMod
             deriving (Show, Eq, Ord)
$(derive[''ModType])

data ModTarget = ArmorClass
               | Fortitude
               | Reflex
               | Will
               | Strength
               | Constitution
               | Dexterity
               | Intelligence
               | Wisdom
               | Charisma
               | Skill
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

-- manually defining this allows me to order my enums more logically
instance Ord ModTarget where
  compare x y = compare (show x) (show y)

data Modifier = Modifier { name :: String
                         , target :: ModTarget
                         , value :: Int
                         , modType :: ModType
                         } deriving (Show)


class Modifiable a where
  modifiers :: a -> [Modifier]

modsByType :: ModType -> [Modifier] -> [Modifier]
modsByType t m = filter (\mod -> modType mod == t) m

modsByTarget :: ModTarget -> [Modifier] -> [Modifier]
modsByTarget t m  = filter (\mod -> target mod == t) m

mods :: ModTarget -> ModType -> [Modifier] -> [Modifier]
mods a y m = ((modsByTarget a) . (modsByType y)) m

mod :: (Modifiable a) => ModTarget -> ModType -> a -> Int
mod a y c
  | length filteredMods > 0 = maximum $ map value filteredMods
  | otherwise = 0
  where filteredMods = mods a y $ modifiers c

-- Return a list of mods, where each mod has the largest value for its target
-- type pair.
modsForTarget :: ModTarget -> [Modifier] -> [Modifier]
modsForTarget t m = concatMap stack $ groupSortMods $ modsByTarget t m

-- Return an integer value which is the sum of the values of each unique
-- target type pair of modifications.
modForTarget :: ModTarget -> [Modifier] -> Int
modForTarget t m = sum $ map value $ modsForTarget t m

-- Return a list of mods (max size 1), where each mod has the largest value
-- for its target type pair.
modsForTargetType :: ModTarget -> ModType -> [Modifier] -> [Modifier]
modsForTargetType a y m = modsByType y $ modsForTarget a m

-- Return an integer value which is the value of the largest mod for the given
-- target type pair.
modForTargetType :: ModTarget -> ModType -> [Modifier] -> Int
modForTargetType a y m = sum $ map value $ modsForTargetType a y m

stack :: [Modifier] -> [Modifier]
stack m
  | all (\mod -> modType mod == UntypedMod) m == True = m -- untyped mods stack
  | minVal < 0 && maxVal > 0 = [min, max] -- bonus & penalty
  | minVal < 0 && maxVal <= 0 = [min] -- penalty only
  | minVal >= 0 = [max] -- bonus only
  where min = minimum m
        minVal = value $ min
        max = maximum m
        maxVal = value $ max

groupSortMods :: [Modifier] -> [[Modifier]]
groupSortMods m = groupBy (stackable) $ sort m

stackable :: Modifier -> Modifier -> Bool
stackable m1 m2 = target m1 == target m2 && modType m1 == modType m2

modFactory name target value modType =
  Modifier { Modifier.name=name
           , Modifier.target=target
           , Modifier.value=value
           , Modifier.modType=modType
           }

instance Eq Modifier where
  x == y = stackable x y && value x == value y

instance Ord Modifier where
  compare x y
    | targetsMatch == False = compare (target x) (target y)
    | typesMatch == False = compare (modType x) (modType y)
    | otherwise = compare (value x) (value y)
    where targetsMatch = target x == target y
          typesMatch = modType x == modType y


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
