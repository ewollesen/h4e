module Equipment where

import Modifier
import Taggable


data Equipment = Equipment { name :: String
                           , modifiers :: [Modifier]
                           , tags :: [Tag]
                           } deriving (Show)

equipFactory n m t = Equipment { Equipment.name=n
                               , Equipment.modifiers=m
                               , Equipment.tags=t
                               }

lightArmorTag = tagFactory "Light"
heavyArmorTag = tagFactory "Heavy"
armorTag = tagFactory "Armor"

plateMail = equipFactory "Plate mail"
            [modFactory "+8 AC bonus (Armor)" ArmorClass 8 ArmorMod,
             modFactory "-1 Speed penalty (Armor)" Speed (-1) ArmorMod,
             modFactory "-2 Armor penalty" Skill (-2) ArmorMod]
            [heavyArmorTag, armorTag]

scaleMail = equipFactory "Scale mail"
            [modFactory "+7 AC bonus (Armor)" ArmorClass 7 ArmorMod,
             modFactory "+2 untyped test" ArmorClass 2 UntypedMod,
             modFactory "-1 Speed penalty (Armor)" Speed (-1) ArmorMod]
            [heavyArmorTag, armorTag]

lightShield = equipFactory "Shield (light)"
              [modFactory "+1 AC bonus (Shield)" ArmorClass 1 ShieldMod,
               modFactory "+3 untyped test" ArmorClass 3 UntypedMod,
               modFactory "+1 Reflex bonus (Shield)" Reflex 1 ShieldMod]
              [lightArmorTag]

instance Taggable Equipment where
  tags e = Equipment.tags e

instance Modifiable Equipment where
  modifiers e = Equipment.modifiers e
