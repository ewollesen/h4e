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
               | Skill
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
               deriving (Show, Eq, Ord)
$(derive[''ModTarget])

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
-- type pair. Will not work for penalties. Will not handle multiple
-- UntypedMods.
modsForTarget :: ModTarget -> [Modifier] -> [Modifier]
modsForTarget t m = map maximum (groupMods (modsByTarget t m))

-- Return an integer value which is the sum of the values of each unique
-- target type pair of modifications. Will not work for penalties. Will not
-- handle multiple UntypedMods.
modForTarget :: ModTarget -> [Modifier] -> Int
modForTarget t m = sum $ map value $ modsForTarget t m

-- Return a list of mods (max size 1), where each mod has the largest value
-- for its target type pair. Will not work for penalties. Will not handle
-- multiple UntypedMods.
modsForTargetType :: ModTarget -> ModType -> [Modifier] -> [Modifier]
modsForTargetType a y m = modsByType y $ modsForTarget a m

-- Return an integer value which is the value of the largest mod for the given
-- target tyep pair. Will not work for penalties. Will not handle multiple
-- UntypedMods.
modForTargetType :: ModTarget -> ModType -> [Modifier] -> Int
modForTargetType a y m = sum $ map value $ modsForTargetType a y m


-- Remove non-stacking mods, keeping only the greatest positive and greatest
-- negative mods.
--groupMaxMinUntyped :: [Modifier] -> [Modifier]
--groupMaxMinUntyped m =

-- groupMaxMinUntyped :: [Modifier] -> [Modifier]
-- groupMaxMinUntyped m =
--   (getUntyped m) ++ (map greatestBonusPenalty (groupMods (getTyped m)))

greatestBonusPenalty :: [Modifier] -> [Modifier]
greatestBonusPenalty m
  | minVal < 0 && maxVal > 0 = [min, max] -- bonus & penalty
  | minVal < 0 && maxVal <= 0 = [min] -- penalty only
  | minVal >= 0 = [max] -- bonus only
  where min = minimum m
        minVal = value $ min
        max = maximum m
        maxVal = value $ max

getUntyped :: [Modifier] -> [Modifier]
getUntyped m = filter (\mod -> modType mod == UntypedMod) m

getTyped :: [Modifier] -> [Modifier]
getTyped m = filter (\mod -> modType mod /= UntypedMod) m

groupUntyped :: [Modifier] -> ([Modifier], [Modifier])
groupUntyped m = partition (untyped) m

groupMods :: [Modifier] -> [[Modifier]]
groupMods m = groupBy (stackable) m

stackable :: Modifier -> Modifier -> Bool
stackable m1 m2 = target m1 == target m2 && modType m1 == modType m2

untyped :: Modifier -> Bool
untyped m = modType m == UntypedMod

sortByTargetType :: [Modifier] -> [Modifier]
sortByTargetType m = sortBy (targetTypeCompare) m

targetTypeCompare m1 m2
  | stackable m1 m2 == True = EQ
  | compareType == EQ = compareTarget
  | otherwise = compareType
  where compareTarget = compare (target m1) (target m2)
        compareType = compare (modType m1) (modType m2)

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
    | x == y = EQ
    | stackable x y && value x < value y = LT
    | stackable x y && value x > value y = GT


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
