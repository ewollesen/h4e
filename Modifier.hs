module Modifier where

data ModType = AbilityMod
             | ArmorMod
             | ClassMod
             | EnhancementMod
             | FeatMod
             | ShieldMod
             | UntypedMod
             deriving (Show, Eq)

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