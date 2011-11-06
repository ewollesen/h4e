module Feat where

import Modifier


data Feat = Feat { name :: String
                 , modifiers :: [Modifier]
                 } deriving (Show)


featFactory n m = Feat { Feat.name=n
                       , Feat.modifiers=m
                       }

psionicToughness = featFactory "Psionic Toughness"
                   [modFactory "+3 HP (Feat)" "Hit Points" 3 FeatMod]