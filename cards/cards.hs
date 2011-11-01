{-# OPTIONS_GHC
    -XFlexibleInstances
    -XOverlappingInstances
    -XMultiParamTypeClasses
    -XFlexibleContexts
    -XUndecidableInstances
    -XTemplateHaskell
    -XDeriveDataTypeable
    -cpp #-}


import Text.RJson
import Data.Generics.SYB.WithClass.Basics
import Data.Generics.SYB.WithClass.Instances
import Data.Generics.SYB.WithClass.Context
import Data.Generics.SYB.WithClass.Derive


data Suit = Clubs
          | Diamonds
          | Hearts
          | Spades
          deriving (Show, Eq, Enum)
data Deck = Deck { cards :: [Suit] } deriving (Show)


$(derive[''Suit])
$(derive[''Deck])


suitToString :: String -> String
suitToString suit = suit

instance Data ToJsonD Suit => ToJson Suit where
  toJson = (enumToJson suitToString)

main = do
  let d1 = Deck { cards = [Clubs, Diamonds] }
  putStrLn $ toJsonString d1