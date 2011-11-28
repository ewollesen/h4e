module Equipment where

import Modifier
import Taggable
import Equippable


data Equipment = Equipment { name :: String
                           , modifiers :: [Modifier]
                           , tags :: [Tag]
                           , proficiencyBonus :: Maybe Int
                           } deriving (Show, Ord, Eq)


instance Taggable Equipment where
  tags e = Equipment.tags e

instance Modifiable Equipment where
  modifiers e = Equipment.modifiers e

instance Equippable Equipment where
  name e = Equipment.name e
  modifiers e = Equipment.modifiers e


equipFactory n m t b = Equipment { Equipment.name=n
                                 , Equipment.modifiers=m
                                 , Equipment.tags=t
                                 , Equipment.proficiencyBonus=b
                                 }

lightArmorTag = tagFactory "Light"
heavyArmorTag = tagFactory "Heavy"
armorTag = tagFactory "Armor"
weaponTag = tagFactory "Weapon"
exoticWeaponTag = tagFactory "Exotic"
martialWeaponTag = tagFactory "Martial"
simpleWeaponTag = tagFactory "Simple"
meleeWeaponTag = tagFactory "Melee"
rangedWeaponTag = tagFactory "Ranged"
magicItemTag = tagFactory "Magic"
neckTag = tagFactory "Neck"

longsword = equipFactory "Longsword"
            []
            [weaponTag, martialWeaponTag, meleeWeaponTag]
            (Just 3)

dagger = equipFactory "Dagger"
         []
         [weaponTag, simpleWeaponTag, meleeWeaponTag, rangedWeaponTag]
         (Just 2)

bastardsword = equipFactory "Bastard sword"
               []
               [weaponTag, meleeWeaponTag, exoticWeaponTag]
               (Just (-99)) -- so I know this value is not verified

plateMail = equipFactory "Plate mail"
            [modFactory "+8 AC bonus (Armor)" ArmorClass 8 ArmorMod,
             modFactory "-1 Speed penalty (Armor)" Speed (-1) ArmorMod,
             modFactory "-2 Armor penalty" Skill (-2) ArmorMod]
            [heavyArmorTag, armorTag]
            Nothing

scaleMail = equipFactory "Scale mail"
            [modFactory "+7 AC bonus (Armor)" ArmorClass 7 ArmorMod,
             modFactory "-1 Speed penalty (Armor)" Speed (-1) ArmorMod]
            [heavyArmorTag, armorTag]
            Nothing

lightShield = equipFactory "Shield (light)"
              [modFactory "+1 AC bonus (Shield)" ArmorClass 1 ShieldMod,
               modFactory "+1 Reflex bonus (Shield)" Reflex 1 ShieldMod]
              [lightArmorTag]
              Nothing

advKit = equipFactory "Adventurer's Kit"
         []
         []
         Nothing