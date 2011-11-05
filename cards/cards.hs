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


data Value = Ten
           | Jack
           | Queen
           | King
           | Ace
           deriving (Show, Eq, Enum)

data Suit = Clubs
          | Diamonds
          | Hearts
          | Spades
          deriving (Show, Eq, Enum)

type Card = (Value, Suit)

data Deck = Deck { cards :: [Card] } deriving (Show)


$(derive[''Value])
$(derive[''Suit])
{-$(derive[''Card])-}
$(derive[''Deck])


suitToString :: String -> String
suitToString suit = suit

valueToString :: String -> String
valueToString value = value

instance Data ToJsonD Suit => ToJson Suit where
  toJson = (enumToJson suitToString)

instance (Data FromJsonD Suit, TranslateField Suit) => FromJson Suit where
    fromJson = (enumFromJson suitToString)

instance Data ToJsonD Value => ToJson Value where
  toJson = (enumToJson valueToString)

instance (Data FromJsonD Value, TranslateField Value) => FromJson Value where
  fromJson = (enumFromJson valueToString)

getIt :: Either String a -> a
getIt c = case c of Right x -> x
                    Left e -> error e

main = do
  let d1 = Deck { cards = [(Ten, Clubs), (Ace, Diamonds)] }
  let json = toJsonString d1
  putStrLn json
  let d1' =  getIt $ fromJsonString (undefined :: Deck) json
  putStrLn $ show d1'
