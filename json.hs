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
import System (getArgs)
import Equipment
import Modifier
import Taggable
import System.IO
import Character
import Weapon
import Level
import Race
import CharacterClass
import Power
import Skill
import Ability
import CSheet
import Feat
import Pontus


$(derive[''Race])
$(derive[''Weapon])
$(derive[''Class])
$(derive[''Level])
$(derive[''Equipment])
$(derive[''Modifier])
$(derive[''Tag])
$(derive[''Power])
$(derive[''Character])
$(derive[''Feat])


getIt :: Either String a -> a
getIt c = case c of Right x -> x
                    Left e -> error e

saveToJson record filename = do
  writeFile filename $ toJsonString record

printCSheet :: String -> IO ()
printCSheet filename = do
  inh <- openFile filename ReadMode
  jsonData <- hGetContents inh
  let character = getIt $ fromJsonString (undefined :: Character) jsonData
  csheet character
  hClose inh

main = do
  args <- getArgs
  let filename = head args
  saveToJson pontus filename
  printCSheet filename
