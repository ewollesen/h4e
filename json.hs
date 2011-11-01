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


-- data Foo = Foo { word :: String } deriving (Show)
-- $(derive[''Foo])

-- a = Foo { word="bar" }
-- b = toJsonString a
-- c = fromJsonString (undefined :: Foo) b


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

dumpIt e f = do
  writeFile f $ toJsonString e

loadIt :: String -> IO ()
loadIt f = do
  inh <- openFile f ReadMode
  jsonData <- hGetContents inh
  putStrLn jsonData
  let pontus' = getIt $ fromJsonString (undefined :: Character) jsonData
  csheet pontus'
  putStrLn $ show $ Skill.trainedSkills pontus'
  hClose inh

main = do
  --putStrLn $ toJsonString pontus
  dumpIt pontus "pontus.json"
  loadIt "pontus.json"
