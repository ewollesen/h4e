module CharacterClass.Battlemind where

import qualified CharacterClass as CC
import Modifier

battlemind :: Modifier -> CC.Class
battlemind mod = CC.Class { CC.name="battlemind"
                          , CC.hpAtFirstLevel=12
                          , CC.hpPerLevelGained=6
                          , CC.healingSurgesPerDay=12
                          , CC.modifiers=[mod]
                          }
