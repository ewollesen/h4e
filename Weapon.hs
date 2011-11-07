module Weapon where

import Equippable
import Equipment
import Modifier
import Taggable
import Weaponlike


data Weapon = Weapon { name :: String
                     , modifiers :: [Modifier]
                     , tags :: [Tag]
                     , proficiencyBonus :: Int
                     } deriving (Show)

weaponFactory n m t p = Weapon { Weapon.name=n
                               , Weapon.modifiers=m
                               , Weapon.tags=t
                               , Weapon.proficiencyBonus=p
                               }

-- is this necessary?
instance Taggable Weapon where
  tags e = Weapon.tags e


weaponTag = tagFactory "Weapon"
exoticWeaponTag = tagFactory "Exotic"
martialWeaponTag = tagFactory "Martial"
simpleWeaponTag = tagFactory "Simple"
meleeWeaponTag = tagFactory "Melee"
rangedWeaponTag = tagFactory "Ranged"


longsword = weaponFactory "Longsword"
            []
            [weaponTag, martialWeaponTag, meleeWeaponTag]
            3

dagger = weaponFactory "Dagger"
         []
         [weaponTag, simpleWeaponTag, meleeWeaponTag, rangedWeaponTag]
         2

bastardsword = weaponFactory "Bastard sword"
               []
               [weaponTag, meleeWeaponTag, exoticWeaponTag]
               (-99) -- so I know this value is not verified
