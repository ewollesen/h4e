module Modifier where

data ModType = AbilityMod
             | ArmorMod
             | ClassMod
             | EnhancementMod
             | FeatMod
             | ShieldMod
             | UntypedMod
             deriving (Show, Eq)

{- TODO ModTarget enum
data ModTarget = Acrobatics
               | ArmorClass
               | Dexterity
               deriving (Show, Eq)
-}

data Modifier = Modifier { name :: String
                         , target :: String -- TODO make me an enum
                         , value :: Int
                         , modType :: ModType
                         } deriving (Show)


class Modifiable a where
  modifiers :: a -> [Modifier]

  modsByType :: a -> ModType -> [Modifier]
  modsByType a t = filter (\mod -> modType mod == t) $ modifiers a

  modsByTarget :: a -> String -> [Modifier]
  modsByTarget a t = filter (\mod -> target mod == t) $ modifiers a

  modsByTargetAndType :: a -> String -> ModType -> [Modifier]
  modsByTargetAndType a desiredTarget desiredModType =
    filter (\mod -> target mod == desiredTarget && modType mod == desiredModType) $ modifiers a

modFactory name target value modType =
  Modifier { Modifier.name=name
           , Modifier.target=target
           , Modifier.value=value
           , Modifier.modType=modType
           }

instance Eq Modifier where
  x == y = target x == target y && modType x == modType y && value x == value y

instance Ord Modifier where
  x < y = target x == target y && modType x == modType y && value x < value y