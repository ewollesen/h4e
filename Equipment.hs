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

plateMail = equipFactory "Plate mail"
            [modFactory "+8 AC bonus (Armor)" "AC" 8,
             modFactory "-1 Speed penalty (Armor)" "Speed" (-1),
             modFactory "-2 Armor penalty" "ArmorPenalty" (-2)]
            [heavyArmorTag]

scaleMail = equipFactory "Scale mail"
            [modFactory "+7 AC bonus (Armor)" "AC" 7,
             modFactory "-1 Speed penalty (Armor)" "Speed" (-1)]
            [heavyArmorTag]

lightShield = equipFactory "Shield (light)"
              [modFactory "+1 AC bonus (Shield)" "AC" 1,
               modFactory "+1 Reflex bonus (Shield)" "Reflex" 1]
              [lightArmorTag]

instance Taggable Equipment where
  tags e = Equipment.tags e
