{-# OPTIONS_GHC
    -XTemplateHaskell
    -XFlexibleInstances
    -XMultiParamTypeClasses
    -XFlexibleContexts
    -XUndecidableInstances #-}
import Text.RJson
import Data.Generics.SYB.WithClass.Basics
import Data.Generics.SYB.WithClass.Derive
import Text.Printf
import Control.Monad
import Equipment
import Modifier
import Taggable
import System.IO
import Pontus
import Character
import Weapon
import Level
import Race
import CharacterClass
import Power
import Skill
import Ability
import CSheet


data Foo = Foo { word :: String } deriving (Show)
$(derive[''Foo])

a = Foo { word="bar" }
b = toJsonString a
c = fromJsonString (undefined :: Foo) b

getIt :: Either String a -> a
getIt c = case c of Right x -> x
                    Left e -> error e


$(derive[''Race])
$(derive[''Weapon])
$(derive[''Class])
$(derive[''Level])
$(derive[''Equipment])
$(derive[''Modifier])
$(derive[''Tag])
$(derive[''Power])
$(derive[''AbilityName])
$(derive[''SkillName])
$(derive[''Character])

--plateMail
--print $ toJsonString plateMail

pm = toJsonString plateMail
pm' = getIt $ fromJsonString (undefined :: Equipment) pm

dumpIt e f = do
  writeFile f $ toJsonString e

loadIt :: String -> IO ()
loadIt f = do
  inh <- openFile f ReadMode
  jsonData <- hGetContents inh
  let p' = getIt $ fromJsonString (undefined :: Character) jsonData
  csheet p'
  hClose inh

--  x <- readFile f
--  putStr x

--loadIt e f = getIt $ fromJsonString e (readIt f)
