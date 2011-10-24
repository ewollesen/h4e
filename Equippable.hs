module Equippable where

import Modifier
import Taggable


class (Taggable a) => Equippable a where
  name :: a -> String
  modifiers :: a -> [Modifier]

