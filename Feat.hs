module Feat where

import Modifier


data Feat = Feat { name :: String
                 , modifiers :: [Modifier]
                 } deriving (Show, Ord, Eq)


featFactory n m = Feat { Feat.name=n
                       , Feat.modifiers=m
                       }

psionicToughness = featFactory "Psionic Toughness"
                   [modFactory "+3 HP (Feat)" HitPoints 3 FeatMod]

lureOfIron = featFactory "Lure of Iron" []
deceptiveMind = featFactory "Deceptive Mind" []