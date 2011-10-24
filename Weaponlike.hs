module Weaponlike where

import Equippable

class (Equippable a) => Weaponlike a where
  proficiencyBonus :: a -> Int

